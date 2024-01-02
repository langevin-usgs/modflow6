"""

Model based on Beg et al. 2022.  It consists of a single
segment discretized into 21 reaches.  There is inflow into
reach 1 based on time series input and outflow to a constant
stage cell in reach 21.  The simulation is for one day using
time steps of 600 seconds (144 time steps). 

"""

import os

import flopy
import numpy as np
import pytest
from conftest import project_root_path
from framework import TestFramework

cases = ["swf-beg2022",]
data_path = project_root_path / "autotest/data/beg2022/"

def build_models(idx, test):

    sim_ws = test.workspace
    name = "swfmodel"
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=sim_ws,
        memory_print_option='all',
    )

    hr2sec = 60. * 60.
    dt = 600 # seconds
    perlen = 24 * hr2sec
    nstp = perlen / dt
    perioddata = [(0.0, 1, 1.0), (perlen, nstp, 1.0)]
    tdis = flopy.mf6.ModflowTdis(
        sim,
        time_units="SECONDS",
        nper=len(perioddata),
        perioddata=perioddata
    )
    ims = flopy.mf6.ModflowIms(
        sim, 
        # no_ptcrecord=True,
        print_option="all", 
        # under_relaxation="DBD",
        # under_relaxation_theta=0.9,
        # under_relaxation_kappa=0.0001,
        # under_relaxation_gamma=0.0,
        # backtracking_number=200,
        # backtracking_tolerance=1.1,
        # backtracking_reduction_factor=0.2,
        # backtracking_residual_limit=1.0,
        linear_acceleration="BICGSTAB",
        outer_dvclose=1.e-4,
        inner_dvclose=1.e-4
    )
    swf = flopy.mf6.ModflowSwf(sim, modelname=name, save_flows=True)

    total_length = 21000
    dx = 1000.
    nreach = int(total_length / dx)
    vertices = []
    vertices = [[j, j * dx, 0., 0.] for j in range(nreach + 1)]
    cell2d = []
    for j in range(nreach):
        cell2d.append([j, 0.5, 2, j, j + 1])
    toreach = [j + 1 for j in range(nreach - 1)] + [-1]
    nodes = len(cell2d)
    nvert = len(vertices)

    slope = 1./10000.
    x = np.linspace(dx / 2, total_length - dx / 2, nreach)
    z = (total_length - x) * slope

    disl = flopy.mf6.ModflowSwfdisl(
        swf, 
        nodes=nodes, 
        nvert=nvert,
        reach_length=dx,
        reach_bottom=z,
        #toreach=toreach,   # -1 gives 0 in one-based, which means outflow cell
        idomain=1, 
        vertices=vertices, 
        cell2d=cell2d,
    )
    
    dfw = flopy.mf6.ModflowSwfdfw(
        swf, 
        central_in_space=True,
        print_flows=True,
        save_flows=True,
        width=40., 
        manningsn=1./80.,
        slope=slope,
        idcxs=0,
    )

    sto = flopy.mf6.ModflowSwfsto(
        swf,
        save_flows=True,
        steady_state={0: True, 1: False},
        transient={0: False, 1: True},
    )

    water_depth = 4.0
    strt = z + water_depth
    ic = flopy.mf6.ModflowSwfic(swf, strt=strt)

    xfraction = np.array([0., 0., 10., 15., 25., 30., 40., 40.]) / 40.
    height = [40., 10., 10., 0., 0., 10., 10., 40.]
    npts = len(height)
    mannfraction = npts * [1.]
    cxsdata = list(zip(xfraction, height, mannfraction))
    cxs = flopy.mf6.ModflowSwfcxs(
        swf,
        nsections=1,
        npoints=npts,
        packagedata=[(0, npts)],
        crosssectiondata=cxsdata,
    )

    # output control
    oc = flopy.mf6.ModflowSwfoc(
        swf,
        budget_filerecord=f"{name}.bud",
        stage_filerecord=f"{name}.stage",
        saverecord=[("STAGE", "ALL"), ("BUDGET", "ALL"), ],
        printrecord=[("STAGE", "LAST"),("BUDGET", "ALL"), ],
    )


    # time, reach1 (cms)
    reach_inflow = [
        (0, 20.),
        (2 * hr2sec, 20),
        (3 * hr2sec, 25),
        (4 * hr2sec, 20),
        (24 * hr2sec, 20),
    ]
    flwlist = [
        [(0,), "reach1"],
    ]
    flw = flopy.mf6.ModflowSwfflw(
        swf,
        maxbound=len(flwlist),
        print_input=True,
        print_flows=True,
        stress_period_data=flwlist,
    )
    filename = name + ".flw.ts"
    time_series_namerecord = [("reach1")]
    interpolation_methodrecord = [("linearend")]
    flw.ts.initialize(
        filename=filename,
        timeseries=reach_inflow,
        time_series_namerecord=time_series_namerecord,
        interpolation_methodrecord=interpolation_methodrecord,
    )

    chd = flopy.mf6.ModflowSwfchd(
        swf,
        maxbound=1,
        print_input=True,
        print_flows=True,
        stress_period_data=[(nreach - 1, z[-1] + water_depth)]
    )

    return sim, None


def make_plot(test, mfsim):
    print("making plots...")
    import matplotlib.pyplot as plt
    import pandas as pd

    hecras = data_path / "hecras0125.csv"
    df_hecras = pd.read_csv(hecras, index_col=False)
    print(df_hecras)

    swrdata = data_path / "mfswr0125.csv"
    df_mfswr = pd.read_csv(swrdata, index_col=False)
    df_mfswr = df_mfswr.loc[df_mfswr['RCHGRP'] == 21]
    print(df_mfswr)

    fpth = os.path.join(test.workspace, f"swfmodel.bud")
    budobj = flopy.utils.binaryfile.CellBudgetFile(fpth, precision="double")
    flowja = budobj.get_data(text="FLOW-JA-FACE")
    qstorage = budobj.get_data(text="STORAGE")
    qflw = budobj.get_data(text="FLW")
    qchd = budobj.get_data(text="CHD")

    qoutflow = []
    times = np.array(budobj.times)
    for ra in qchd:
        q = - ra[0]["q"]
        qoutflow.append(q)

    qinflow = []
    for ra in qflw:
        q = ra[0]["q"]
        qinflow.append(q)

    # plot upstream and downstream flow
    fig = plt.figure(figsize=(8, 6))
    ax = fig.add_subplot(1, 1, 1)
    ax.plot(times / 60. / 60., qinflow, 'r-', label="Inflow")
    ax.plot(times / 60. / 60., df_hecras["Flow Flow (CMS)"], 'b-', label="HEC-RAS")
    x = df_mfswr["TOTTIME"] - 86400.
    x = x / 60. / 60.
    ax.plot(x, -df_mfswr["QCRFLOW"], 'go', mfc="none", label="MODFLOW-SWR")
    ax.plot(times / 60. / 60., qoutflow, 'bo', mfc="none", label="MODFLOW 6")
    ax.set_xlim(0, 24.)
    ax.set_ylim(19, 26)
    plt.xlabel("time, in hours")
    plt.ylabel("flow, in meters cubed per second")
    plt.legend()
    fname = os.path.join(test.workspace, "swfmodel.flow.png")
    plt.savefig(fname)

    # read and plot stages
    fpth = os.path.join(test.workspace, "swfmodel.stage")
    qobj = flopy.utils.HeadFile(fpth, precision="double", text="STAGE")
    stage = qobj.get_alldata()

    fig = plt.figure(figsize=(10, 10))
    ax = fig.add_subplot(1, 1, 1)
    ax.plot(times / 60. / 60., stage[:, 0, 0, 0], 'r-', label="Upstream")
    ax.plot(times / 60. / 60., stage[:, 0, 0, -1], 'bo', mfc="none", label="Downstream")
    # ax.set_xlim(0, 24.)
    # ax.set_ylim(19, 26)
    plt.xlabel("time, in hours")
    plt.ylabel("stage, in meters")
    plt.legend()
    fname = os.path.join(test.workspace, "swfmodel.stage.png")
    plt.savefig(fname)


    return


def check_output(idx, test):
    print("evaluating model...")

    # get MFSimulation from test
    sim = test.sims[0]

    makeplot = True
    if make_plot:
        make_plot(test, sim)

    # assign name
    name = "swfmodel"

    # read the binary grid file
    fpth = os.path.join(test.workspace, f"{name}.disl.grb")
    grb = flopy.mf6.utils.MfGrdFile(fpth)
    ia = grb.ia
    ja = grb.ja
    assert ia.shape[0] == grb.nodes + 1, "ia in grb file is not correct size"

    # read stage file
    fpth = os.path.join(test.workspace, f"{name}.stage")
    qobj = flopy.utils.HeadFile(fpth, precision="double", text="STAGE")
    stage = qobj.get_alldata()

    # read the budget file
    fpth = os.path.join(test.workspace, f"{name}.bud")
    budobj = flopy.utils.binaryfile.CellBudgetFile(fpth, precision="double")
    flowja = budobj.get_data(text="FLOW-JA-FACE")
    qstorage = budobj.get_data(text="STORAGE")
    qflw = budobj.get_data(text="FLW")
    qchd = budobj.get_data(text="CHD")
    qresidual = np.zeros(grb.nodes)

    # # check budget terms
    # for itime in range(len(flowja)):
    #     print (f"evaluating timestep {itime}")

    #     fja = flowja[itime].flatten()
    #     for n in range(grb.nodes):
    #         ipos = ia[n]
    #         qresidual[n] = fja[ipos]
    #     assert np.allclose(qresidual, 0.), "residual in flowja diagonal is not zero"

    #     for n in range(grb.nodes):
    #         qs = qstorage[itime].flatten()[n]
    #         if n + 1 in qflw[itime]["node"]:
    #             idx, = np.where(qflw[itime]["node"] == n + 1)
    #             idx = idx[0]
    #             qf = qflw[itime].flatten()["q"][idx]
    #         else:
    #             qf = 0.
    #         qe = qextoutflow[itime].flatten()[n]
    #         qdiag = fja[ia[n]]
    #         print(f"{n=} {qs=} {qf=} {qe=} {qdiag=}")
    #         for ipos in range(ia[n] + 1, ia[n + 1]):
    #             j = ja[ipos]
    #             q = fja[ipos]
    #             print(f"  {ipos=} {j=} {q=}")        

    return


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
