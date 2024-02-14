"""

The purpose of this test is to recreate the rectangular
cross section simulations reported by Todini (2007) using the
SWF DFW and MCT Packages.  The model consists of a 50-reach stream
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

from cross_section_functions import qtodepth
from framework import TestFramework

cases = [f"swf-tod{i}" for i in range(20)]

# cross section data
xfraction = [0.0, 0.0, 1.0, 1.0]
height = [100.0, 0.0, 0.0, 100.0]
mannfraction = [1.0, 1.0, 1.0, 1.0]
cxsdata = list(zip(xfraction, height, mannfraction))

# answers from MCT paper for rectangular cross section simulation with varying slopes
qmax_answer = [
    894.68,
    879.10,
    819.78,
    669.53,
    423.11,
    873.19,
    801.63,
    669.53,
    630.09,
    505.99,
    669.51,
    669.53,
    669.62,
    675.69,
    675.92,
    669.65,
    669.53,
    669.15,
    669.55,
    668.43,
]

base_parameters = {
    "slope": 0.00025,
    "manning": 0.035,
    "dx": 2000.0,
    "dt": 1800,
}
slope_test = [0.002, 0.001, 0.0005, 0.00025, 0.0001]
manning_test = [0.01, 0.02, 0.035, 0.04, 0.06]
dx_test = [1000.0, 2000.0, 4000.0, 6000.0, 8000.0]
dt_test = [900.0, 1800.0, 3600.0, 5400.0, 7200.0]
test_dict = {}
params = [
    ("slope", slope_test),
    ("manning", manning_test),
    ("dx", dx_test),
    ("dt", dt_test),
]
itest = 0
for param_name, param in params:
    for pval in param:
        d = base_parameters.copy()
        d[param_name] = pval
        test_dict[itest] = d
        itest += 1


def get_inflow_hydrograph(Qbase, Qpeak, Tp, beta, t):
    return Qbase + (Qpeak - Qbase) * (t / Tp * np.exp(1 - t / Tp)) ** beta


def build_models(idx, test):

    test_params = test_dict[idx]
    slope = test_params["slope"]
    manning = test_params["manning"]
    dx = test_params["dx"]
    dt = test_params["dt"]

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

    # timing
    # dt = 1800. # seconds
    perlen = 345600.0
    total_time = perlen
    nstp = int(total_time / dt)

    # inflow hydrograph
    beta = 16
    Qbase = 100  # m^3/s
    Qpeak = 900  # m^3/s
    Tp = 24 * 60 * 60  # seconds
    times = np.arange(0, total_time + dt, dt)
    inflow_hydrograph = get_inflow_hydrograph(Qbase, Qpeak, Tp, beta, times)

    # dx = 2000.
    total_length = 50 * 2000.0
    nreach = int(total_length / dx)
    vertices = []
    vertices = [[j, j * dx, 0.0, 0.0] for j in range(nreach + 1)]
    cell2d = []
    for j in range(nreach):
        cell2d.append([j, 0.5, 2, j, j + 1])
    toreach = [j + 1 for j in range(nreach - 1)] + [-1]
    nodes = len(cell2d)
    nvert = len(vertices)

    reach_bottom = np.zeros(nodes, dtype=float)
    reach_bottom[0] = (nodes - 1) * dx * slope
    for n in range(1, nreach):
        reach_bottom[n] = reach_bottom[n - 1] - dx * slope

    # depth0 needs to be calculated to give flow of 100.
    initial_flow = 100.0
    width = 50.0
    depth0 = qtodepth(
        np.array(xfraction) * width,
        np.array(height),
        initial_flow,
        np.array(mannfraction) * manning,
        slope=slope,
    )
    stage0 = reach_bottom + depth0

    tdis = flopy.mf6.ModflowTdis(
        sim, nper=1, perioddata=[(perlen, nstp, 1.0)], time_units="seconds"
    )

    for routing_package in ["dfw", "mct"]:

        name = f"model_{routing_package}"

        if routing_package == "dfw":
            ims_dfw = flopy.mf6.ModflowIms(
                sim,
                filename=f"{name}.ims",
                outer_maximum=300,
                outer_dvclose=0.001,
                inner_dvclose=1.0e-6,
                linear_acceleration="BICGSTAB",
                print_option="ALL",
                relaxation_factor=0.0,
            )

        elif routing_package == "mct":
            ems_mct = flopy.mf6.ModflowEms(
                sim,
                filename=f"{name}.ems",
            )

        swf = flopy.mf6.ModflowSwf(
            sim, modelname=name, save_flows=True, print_flows=True
        )

        if routing_package == "dfw":
            sim.register_ims_package(ims_dfw, [name])
        elif routing_package == "mct":
            sim.register_ims_package(ems_mct, [name])

        disl = flopy.mf6.ModflowSwfdisl(
            swf,
            filename=f"{name}.disl",
            nodes=nodes,
            nvert=nvert,
            reach_length=dx,
            reach_bottom=reach_bottom,
            toreach=None if routing_package == "dfw" else toreach,
            idomain=1,
            vertices=vertices,
            cell2d=cell2d,
        )

        if routing_package == "dfw":

            dfw = flopy.mf6.ModflowSwfdfw(
                swf,
                filename=f"{name}.dfw",
                print_flows=True,
                save_flows=True,
                width=50.0,
                manningsn=manning,
                slope=slope,
                idcxs=0,
            )

            fname = f"{name}.zdg.obs.csv"
            zdg_obs = {
                fname: [
                    ("OUTFLOW", "ZDG", (nreach - 1,)),
                ],
                "digits": 10,
            }
            idcxs = 0  # first cross section
            width = 50.0
            slope = slope
            rough = manning
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

            ic = flopy.mf6.ModflowSwfic(
                swf, filename=f"{name}.ic", strt=stage0
            )

        elif routing_package == "mct":

            fname = f"{name}.{routing_package}.obs.csv"
            routing_obs = {
                fname: [
                    ("OUTFLOW", "EXT-OUTFLOW", (nodes - 1,)),
                ],
                "digits": 10,
            }
            mct = flopy.mf6.ModflowSwfmct(
                swf,
                filename=f"{name}.mct",
                observations=routing_obs,
                print_flows=True,
                save_flows=True,
                icalc_order=list(range(nodes)),
                qoutflow0=inflow_hydrograph[0],
                width=50.0,
                manningsn=manning,
                elevation=0.0,
                slope=slope,
                idcxs=0,
            )

        cxs = flopy.mf6.ModflowSwfcxs(
            swf,
            nsections=1,
            npoints=4,
            packagedata=[(0, 4)],
            crosssectiondata=cxsdata,
            filename=f"{name}.cxs",
        )

        if routing_package == "dfw":
            oc = flopy.mf6.ModflowSwfoc(
                swf,
                filename=f"{name}.oc",
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
        else:
            oc = flopy.mf6.ModflowSwfoc(
                swf,
                filename=f"{name}.oc",
                budget_filerecord=f"{name}.bud",
                qoutflow_filerecord=f"{name}.qoutflow",
                saverecord=[
                    ("QOUTFLOW", "ALL"),
                    ("BUDGET", "ALL"),
                ],
                printrecord=[
                    ("QOUTFLOW", "ALL"),
                    ("BUDGET", "ALL"),
                ],
            )

        flw = flopy.mf6.ModflowSwfflw(
            swf,
            filename=f"{name}.flw",
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

    fpth = os.path.join(test.workspace, f"model_dfw.zdg.obs.csv")
    qoutflow_dfw = np.genfromtxt(fpth, names=True, delimiter=",")
    fpth = os.path.join(test.workspace, f"model_mct.mct.obs.csv")
    qoutflow_mct = np.genfromtxt(fpth, names=True, delimiter=",")

    fig = plt.figure(figsize=(10, 10))
    ax = fig.add_subplot(1, 1, 1)
    ax.plot(qoutflow_dfw["time"], -qoutflow_dfw["OUTFLOW"], "k-", label="DFW")
    ax.plot(qoutflow_mct["time"], -qoutflow_mct["OUTFLOW"], "b-", label="MCT")
    # ax.plot(times, inflow_hydrograph, "g-", label="Inflow")
    plt.xlabel("time, in seconds")
    plt.ylabel("flow, in cms")
    plt.legend()
    fname = os.path.join(ws, f"{name}.{idx}.obs.png")
    plt.savefig(fname)

    return


def check_output(idx, test):
    print("evaluating model...")

    makeplot = False
    if makeplot:
        make_plot(idx, test)

    # read the observation output
    name = "model_dfw"
    fpth = os.path.join(test.workspace, f"{name}.zdg.obs.csv")
    obsvals = np.genfromtxt(fpth, names=True, delimiter=",")
    qoutflow = -obsvals["OUTFLOW"]
    qms = qoutflow.max()
    qma = qmax_answer[idx]
    d = abs(qms - qma)
    print(f"qout mf6 ({qms}) todini ({qma}); diff = {d}")
    dtol = 20.0
    assert d < dtol, f"qoutflow; diff {d} > dtol {dtol}." f"  {qms} /= {qma}"

    # read the binary grid file
    fpth = os.path.join(test.workspace, f"{name}.disl.grb")
    grb = flopy.mf6.utils.MfGrdFile(fpth)
    ia = grb.ia
    ja = grb.ja
    assert ia.shape[0] == grb.nodes + 1, "ia in grb file is not correct size"

    # read stage file
    fpth = os.path.join(test.workspace, f"{name}.stage")
    sobj = flopy.utils.HeadFile(fpth, precision="double", text="STAGE")
    stage = sobj.get_alldata()

    # read the budget file
    fpth = os.path.join(test.workspace, f"{name}.bud")
    budobj = flopy.utils.binaryfile.CellBudgetFile(fpth)
    flowja = budobj.get_data(text="FLOW-JA-FACE")
    qstorage = budobj.get_data(text="STORAGE")
    qflw = budobj.get_data(text="FLW")
    qextoutflow = budobj.get_data(text="ZDG")
    qresidual = np.zeros(grb.nodes)

    # check budget terms
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
