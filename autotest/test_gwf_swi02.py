"""
Test of the seawater intrusion (SWI) package for confined,
unconfined, and newton.  This test is patterned after
swi01, but it is transient instead of steady state.

"""

import pathlib as pl

import flopy
import numpy as np
import pytest
from framework import TestFramework

cases = [
    "swi02a",  # confined
    "swi02b",  # unconfined
    "swi02c",  # unconfined newton
]

# should get three cases working here:
#   1: confined, 2: unconfined, 3: unconfined/newton
icelltype = [0, 1, 1]
iconvert = [0, 1, 1]
top = [0.0, 50.0, 50.0]
newtonoptions = [False, False, "newton"]


def build_models(idx, test):
    Lx = 10000  # meters
    delr, delc = 100.0, 1.0
    ncol = int(Lx / delr)
    nlay = 1
    nrow = 1
    botm = -400.0
    recharge = 0.001
    k = 10.0
    h0 = 0.0
    h1 = h0

    ws = test.workspace
    name = "mymodel"
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        sim_ws=ws,
        exe_name="mf6",
        memory_print_option="all",
        print_input=True,
    )

    # transient tdis
    nper = 1
    nstp = 10
    perlen = 100000.0
    perioddata = nper * [(perlen, nstp, 1.0)]
    tdis = flopy.mf6.ModflowTdis(sim, perioddata=perioddata)

    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="all",
        outer_maximum=50,
        linear_acceleration="bicgstab",
    )
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
        save_flows=True,
        newtonoptions=newtonoptions[idx],
    )
    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top[idx],
        botm=botm,
    )
    ic = flopy.mf6.ModflowGwfic(gwf)
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_specific_discharge=True,
        alternative_cell_averaging="harmonic",
        icelltype=icelltype[idx],
        k=k,
    )
    sto = flopy.mf6.ModflowGwfsto(
        gwf, iconvert=iconvert[idx], ss=1.0e-5, sy=0.2
    )
    zeta_file = name + ".zta"
    swi = flopy.mf6.ModflowGwfswi(
        gwf, zeta_filerecord=zeta_file, zetastrt=-1.0
    )
    cghb = 1.0 * delr * delc / 10.0
    ghb = flopy.mf6.ModflowGwfghb(
        gwf,
        stress_period_data=[[0, 0, 0, h0, cghb], [0, 0, ncol - 1, h1, cghb]],
    )
    rch = flopy.mf6.ModflowGwfrcha(gwf, recharge=recharge)
    budget_file = name + ".bud"
    head_file = name + ".hds"
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=budget_file,
        head_filerecord=head_file,
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    return sim, None


def make_plot(sim, idx):
    import matplotlib.pyplot as plt

    ws = pl.Path(sim.sim_path)
    gwf = sim.gwf[0]
    x = gwf.modelgrid.xcellcenters.flatten()
    fpth = pl.Path(ws) / f"{gwf.name}.zta"
    head = gwf.output.head().get_data().flatten()
    zobj = flopy.utils.HeadFile(fpth, text="zeta")
    times = zobj.times

    ax = plt.subplot(1, 1, 1)
    pxs = flopy.plot.PlotCrossSection(gwf, line={"row": 0}, ax=ax)
    for t in times:
        zeta = zobj.get_data(totim=t).flatten()
        ax.plot(x, zeta, "k-")
    ax.plot(x, head, "b-")

    ax.set_ylim(-400, 50.0)
    plt.savefig(ws / "zeta.png")
    plt.close("all")


def check_output(idx, test):
    # get the flopy sim object
    sim = test.sims[0]

    makeplot = True
    if makeplot:
        make_plot(sim, idx)

    ws = pl.Path(sim.sim_path)
    gwf = sim.gwf[0]
    x = gwf.modelgrid.xcellcenters.flatten()
    fpth = pl.Path(ws) / f"{gwf.name}.zta"
    head = gwf.output.head().get_data().flatten()
    zeta = flopy.utils.HeadFile(fpth, text="zeta").get_data().flatten()

    zeta_answer = -head * 40.0
    for j in range(head.shape[0]):
        print(j, head[j], zeta[j], zeta_answer[j])
    assert np.allclose(
        zeta, zeta_answer
    ), f"zeta is not right {zeta} /= {zeta_answer}"


@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        targets=targets,
    )
    test.run()
