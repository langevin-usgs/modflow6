"""

The purpose of this test is to recreate the rectangular
cross section simulations reported by Todini (2007) using the
SWF DFW Package.  The model consists of a 50-reach stream
with a specified inflow.  The simulated outflow from the last
reach (the maximum rate) is compared with the maximum rate
reported by Todini (2007).  The test uses the 5 different slope
values in Table 2 and the reported 5 different Qmax values to
test the SWF DFW implementation.

"""

import os

import flopy
import numpy as np
import pytest

from framework import TestFramework

cases = [f"swf-dfwtod{a}" for a in ["a", "b", "c", "d", "e"]]

# answers from MCT paper for rectangular cross section simulation with varying slopes
qmax_answer = [894.68, 879.10, 819.78, 669.53, 423.11]
slope_test = [0.002, 0.001, 0.0005, 0.00025, 0.0001]
depth0 = [
    1.335960088822219,
    1.6526425510917875,
    2.0466190530314066,
    2.53789174596335,
    3.3813983488855115,
]


def get_inflow_hydrograph(Qbase, Qpeak, Tp, beta, t):
    return Qbase + (Qpeak - Qbase) * (t / Tp * np.exp(1 - t / Tp)) ** beta


def build_models(idx, test):

    sim_ws = test.workspace
    name = cases[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=sim_ws,
        memory_print_option="all",
        print_input=True,
    )

    dt = 1800.0  # seconds
    perlen = 345600.0
    total_time = perlen
    nstp = int(total_time / dt)

    beta = 16
    Qbase = 100  # m^3/s
    Qpeak = 900  # m^3/s
    Tp = 24 * 60 * 60  # seconds
    times = np.arange(0, total_time + dt, dt)
    inflow_hydrograph = get_inflow_hydrograph(Qbase, Qpeak, Tp, beta, times)

    tdis = flopy.mf6.ModflowTdis(
        sim, nper=1, perioddata=[(perlen, nstp, 1.0)], time_units="seconds"
    )
    ims = flopy.mf6.ModflowIms(
        sim,
        outer_maximum=300,
        outer_dvclose=0.001,
        inner_dvclose=1.0e-6,
        linear_acceleration="BICGSTAB",
        print_option="ALL",
        relaxation_factor=0.0,
    )
    swf = flopy.mf6.ModflowSwf(
        sim, modelname=name, save_flows=True, print_flows=True
    )

    dx = 2000.0
    nreach = 50
    total_length = dx * nreach
    vertices = []
    vertices = [[j, j * dx, 0.0, 0.0] for j in range(nreach + 1)]
    cell2d = []
    for j in range(nreach):
        cell2d.append([j, 0.5, 2, j, j + 1])
    toreach = [j + 1 for j in range(nreach - 1)] + [-1]
    nodes = len(cell2d)
    nvert = len(vertices)

    elevation = np.zeros(nodes, dtype=float)
    elevation[0] = (nodes - 1) * dx * slope_test[idx]
    for n in range(1, nreach):
        elevation[n] = elevation[n - 1] - dx * slope_test[idx]
    # depth0 needs to be calculated to give flow of 100.
    stage0 = elevation + depth0[idx]

    disl = flopy.mf6.ModflowSwfdisl(
        swf,
        nodes=nodes,
        nvert=nvert,
        reach_length=dx,
        reach_bottom=elevation,
        # toreach=toreach,   # -1 gives 0 in one-based, which means outflow cell
        idomain=1,
        vertices=vertices,
        cell2d=cell2d,
    )

    dfw = flopy.mf6.ModflowSwfdfw(
        swf,
        print_flows=True,
        save_flows=True,
        width=50.0,
        manningsn=0.035,
        slope=slope_test[idx],
        idcxs=0,
    )

    # note: for specifying zero-based reach number, put reach number in tuple
    fname = f"{name}.zdg.obs.csv"
    zdg_obs = {
        fname: [
            ("OUTFLOW", "ZDG", (nodes - 1,)),
        ],
        "digits": 10,
    }

    idcxs = -1  # no cross section
    width = 50.0
    slope = slope_test[idx]
    rough = 0.035
    spd = [((nreach - 1,), idcxs, width, slope, rough)]
    zdg = flopy.mf6.ModflowSwfzdg(
        swf,
        observations=zdg_obs,
        maxbound=len(spd),
        stress_period_data=spd,
    )

    sto = flopy.mf6.ModflowSwfsto(
        swf,
        save_flows=True,
    )

    ic = flopy.mf6.ModflowSwfic(swf, strt=stage0)

    xfraction = [0.0, 0.0, 1.0, 1.0]
    height = [100.0, 0.0, 0.0, 100.0]
    mannfraction = [1.0, 1.0, 1.0, 1.0]
    cxsdata = list(zip(xfraction, height, mannfraction))
    cxs = flopy.mf6.ModflowSwfcxs(
        swf,
        nsections=1,
        npoints=4,
        packagedata=[(0, 4)],
        crosssectiondata=cxsdata,
    )

    # output control
    oc = flopy.mf6.ModflowSwfoc(
        swf,
        budget_filerecord=f"{name}.bud",
        stage_filerecord=f"{name}.stage",
        saverecord=[
            ("STAGE", "ALL"),
            ("BUDGET", "ALL"),
        ],
        printrecord=[
            ("STAGE", "ALL"),
            ("BUDGET", "ALL"),
        ],
    )

    # Create flw package with time series for inflow into reach 1
    flw = flopy.mf6.ModflowSwfflw(
        swf,
        maxbound=1,
        print_input=True,
        print_flows=True,
        stress_period_data=[(0, "inflow")],
    )
    fname = f"{name}.flw.ts"
    flw.ts.initialize(
        filename=fname,
        timeseries=list(zip(times, inflow_hydrograph)),
        time_series_namerecord=["inflow"],
        interpolation_methodrecord=["linearend"],
    )

    obs_data = {
        f"{name}.obs.csv": [
            ("UPSTAGE", "STAGE", (0,)),
            ("DOWNSTAGE", "STAGE", (nodes - 1,)),
        ],
    }
    obs_package = flopy.mf6.ModflowUtlobs(
        swf,
        filename=f"{name}.obs",
        digits=10,
        print_input=True,
        continuous=obs_data,
    )

    return sim, None


def make_plot(idx, test):
    print("making plots...")
    import matplotlib.pyplot as plt

    name = test.name
    ws = test.workspace
    mfsim = flopy.mf6.MFSimulation.load(sim_ws=ws)
    swf = mfsim.get_model(name)

    fpth = ws / f"{name}.zdg.obs.csv"
    simvals = np.genfromtxt(fpth, names=True, delimiter=",")

    fig = plt.figure(figsize=(10, 10))
    ax = fig.add_subplot(1, 1, 1)
    ax.plot(simvals["time"], -simvals["OUTFLOW"], "k-", label="mf6")
    ax.plot((0, simvals["time"][-1]), 2 * [qmax_answer[idx]], label="Todini")
    plt.xlabel("time, in seconds")
    plt.ylabel("flow, in cms")
    plt.legend()
    fname = ws / f"{name}.{idx}.obs.png"
    plt.savefig(fname)

    return


def check_output(idx, test):
    print("evaluating model...")

    makeplot = False
    if makeplot:
        make_plot(idx, test)

    # read the observation output
    name = cases[idx]
    fpth = test.workspace / f"{name}.zdg.obs.csv"
    obsvals = np.genfromtxt(fpth, names=True, delimiter=",")
    qoutflow = -obsvals["OUTFLOW"]
    qms = qoutflow.max()
    qma = qmax_answer[idx]
    d = abs(qms - qma)
    print(f"Outflow max mf6 ({qms}) and reported ({qma}); diff = {d}")
    dtol = 0.4
    assert d < dtol, (
        f"Sim and reported max outflow too different; diff {d} > dtol {dtol}."
        f"  {qms} /= {qma}"
    )

    # read the binary grid file
    fpth = test.workspace / f"{name}.disl.grb"
    grb = flopy.mf6.utils.MfGrdFile(fpth)
    ia = grb.ia
    ja = grb.ja
    assert ia.shape[0] == grb.nodes + 1, "ia in grb file is not correct size"

    # read qoutflow file
    fpth = test.workspace / f"{name}.qoutflow"
    qobj = flopy.utils.HeadFile(fpth, precision="double", text="QOUTFLOW")
    qoutflow = qobj.get_alldata()

    # read the budget file
    fpth = test.workspace / f"{name}.bud"
    budobj = flopy.utils.binaryfile.CellBudgetFile(fpth)
    flowja = budobj.get_data(text="FLOW-JA-FACE")
    qstorage = budobj.get_data(text="STORAGE")
    qflw = budobj.get_data(text="FLW")
    qextoutflow = budobj.get_data(text="EXT-OUTFLOW")
    qresidual = np.zeros(grb.nodes)

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
