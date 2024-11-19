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
    "swi04a",
]

dmax = -20. # deepest ocean bottom elevation
dmin = -1. # shallowest ocean bottom elevation
Lx_coast = 3000. # x position of coastline from the left
Lx_edge = 10000. # x position of right edge of model domain
delr = 100.
delc = 1.
nlay = 1
nrow = 1
ncol = int(Lx_edge / delr)
top_island = 10.
botm_aquifer = -200.
recharge = 0.001
hydraulic_conductivity = 100.0
ghb_cond_fact = 0.001
h0 = 0.0
h1 = h0

dx = Lx_edge / ncol 
x = np.linspace(dx / 2., Lx_edge - dx / 2, ncol)
slope = (dmin - dmax) / Lx_coast
top = dmax + x * slope
top[x>Lx_coast] = top_island
top = top.reshape((nrow, ncol))
print(ncol)
print(dx)
print(x) 
print(top)
print(np.where(x < Lx_coast))

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
        top=top,
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
    zeta_file = name + ".zta"
    swi = flopy.mf6.ModflowGwfswi(gwf, zeta_filerecord=zeta_file, zetastrt=0)
    cghb = hydraulic_conductivity * ghb_cond_fact * delr * delc / 1
    jcol_ghb = np.where(x < Lx_coast)[0]
    ghb_list = []
    for j in jcol_ghb:
        freshwater_head = h0 + (h0 - top[0, j]) * 0.025
        # freshwater_head = h0
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


def make_cross_section_plot(sim, idx, title):
    import matplotlib.pyplot as plt

    ws = pl.Path(sim.sim_path)
    gwf = sim.gwf[0]
    x = gwf.modelgrid.xcellcenters.flatten()
    botm = gwf.dis.botm.array.flatten()
    ws = sim.sim_path
    fpth = ws / "mymodel.zta"
    head = gwf.output.head().get_data().flatten()
    zeta = gwf.swi.output.zeta().get_data().flatten()
    #zeta = flopy.utils.HeadFile(fpth, text="zeta").get_data().flatten()
    tp = head
    pxs = flopy.plot.PlotCrossSection(gwf, line={"row": 0})
    ax = pxs.ax
    #ax.plot(x, head, "k-")
    #ax.plot(x, zeta, "k--")

    import matplotlib.patches
    rect = matplotlib.patches.Rectangle((0, botm_aquifer), Lx_edge, h0 -botm_aquifer, fc="red")
    ax.add_patch(rect)

    colors = ["cyan", "red"]
    pxs.plot_fill_between(zeta, head=head, colors=colors, ax=ax, edgecolors="none")

    pxs.plot_grid()
    ax.set_title(title)
    ax.set_ylim(botm_aquifer, top_island)
    #ax.set_aspect(1.0)
    plt.savefig(ws / "zeta.png")
    plt.close("all")

def check_output(idx, test):
    # get the flopy sim object
    sim = test.sims[0]

    makeplot = True
    if makeplot:
        title = {
            0: "Case 4a",
        }
        make_cross_section_plot(sim, idx, title[idx])
#        make_head_plot(sim, idx, title[idx])


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
