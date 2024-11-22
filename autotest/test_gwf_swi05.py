"""
Test of the seawater intrusion (SWI) package for a sloping offshore ghb
boundary.

"""

import pathlib as pl

import flopy
import numpy as np
import pytest
from framework import TestFramework

cases = [
    "swi05a",
    "swi05b",
]
saltwater_head = [0.0, 40.0]

Lx = 10000. # x position of right edge of model domain
delr = 100.
delc = 1.
nlay = 1
nrow = 1
ncol = int(Lx / delr)
top_island = 50.
botm_aquifer = -50.
recharge = 0.0001
hydraulic_conductivity = 100.0
ghb_cond_fact = 1.0


def build_models(idx, test):
    ws = test.workspace
    name = "mymodel"
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        sim_ws=ws,
        exe_name="mf6",
        memory_print_option="all",
        print_input=True,
    )
    tdis = flopy.mf6.ModflowTdis(sim)
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="summary",
        no_ptcrecord=True,
        outer_maximum=500,
        linear_acceleration="bicgstab",
    )
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
        save_flows=True,
        newtonoptions=True,
    )
    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top_island,
        botm=botm_aquifer,
    )
    ic = flopy.mf6.ModflowGwfic(gwf, strt=0)
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_specific_discharge=True,
        # alternative_cell_averaging="amt-hmk",  # "harmonic",
        icelltype=1,
        k=hydraulic_conductivity,
    )
    h0 = saltwater_head[idx]
    zeta_file = name + ".zta"
    swi = flopy.mf6.ModflowGwfswi(
        gwf, 
        zeta_filerecord=zeta_file,
        saltwater_head=h0,
        zetastrt=-10,
    )
    cghb = hydraulic_conductivity * ghb_cond_fact * delr * delc / 1
    ghb_list = []
    for j in [0, ncol - 1]:
        freshwater_head = h0
        ghb_list.append([0, 0, j, freshwater_head, cghb])

    ghb = flopy.mf6.ModflowGwfghb(
        gwf,
        stress_period_data=ghb_list,
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


def make_cross_section_plot(test, sim, idx):
    import matplotlib.pyplot as plt

    ws = pl.Path(sim.sim_path)
    gwf = sim.gwf[0]
    ws = sim.sim_path
    head = gwf.output.head().get_data().flatten()
    zeta = gwf.swi.output.zeta().get_data().flatten()
    volume_fresh = (head - zeta) * delr
    volume_fresh = volume_fresh.sum()
    pxs = flopy.plot.PlotCrossSection(gwf, line={"row": 0})
    ax = pxs.ax

    colors = ["cyan", "red"]
    pxs.plot_fill_between(zeta, head=head, colors=colors, ax=ax, edgecolors="none")

    pxs.plot_grid()
    title = f"saltwater head = {saltwater_head[idx]}; Fresh volume = {volume_fresh:0.2f}"
    ax.set_title(title)
    ax.set_ylim(botm_aquifer, top_island)
    plt.savefig(ws / "zeta.png")
    plt.close("all")


def check_output(idx, test):
    # get the flopy sim object
    sim = test.sims[0]

    makeplot = False
    if makeplot:
        make_cross_section_plot(test, sim, idx)


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
