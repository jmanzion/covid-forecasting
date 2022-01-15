import pandas as pd
import numpy as np
from scipy.integrate import odeint
from scipy.optimize import least_squares
from scipy.stats import poisson
from scipy.stats import nbinom
# import matplotlib.pyplot as plt
# from statsmodels.graphics.gofplots import qqplot
# from pandas.plotting import autocorrelation_plot
import math

def SimpleGrowth(x, t, flag, params):
    '''
    Differential equation governing the growth
    flag: #1-Exp; 2-GGM; 3-Logistic Growth; 4-GLM
    '''
    if flag==1:
        r = params
        dx = r * x
    elif flag==2:
        r, p = params
        dx = r * (x**p)
    elif flag==3:
        r, K = params
        dx = r * x * (1 - (x/K))
    elif flag==4:
        r, p, K = params
        dx = r * (x**p) * (1 - (x/K))
    elif flag==5:
        r, p, a, K = params
        dx = r * (x**p) * (1 - (x/K)**a)
    return dx


def solvedSimpleGrowth(params, IC, tf, flag):
    '''
    This function solves the differential equation in SimpleGrowth
    Parameters
    ----------
    params : a vector of parameters to be optimized
    IC : initial condition of the system
    tf : time vector
    flag : indicate what growth model should be solved

    Returns
    -------
    y : a vector of predicted case incidence

    '''
    x = pd.DataFrame(odeint(SimpleGrowth, IC, tf, args=(flag, params)))
    y = x.diff() #taking predicted incidence from 
    y.iat[0,0] = x.iat[0,0]
    return y


def ResidFun(params, IC, tf, flag, cases):
    '''
    Calculate residual = predicted - actual

    Parameters
    ----------
    params : a vector of parameters to be optimized
    IC : initial condition of the system
    tf : time vector
    flag : indicate what growth model should be solved
    cases : vector of actual case incidence

    Returns
    -------
    resid: a vector of residuals

    '''
    y = solvedSimpleGrowth(params, IC, tf, flag)
    z = y.sub(cases, axis=0)
    resid  = (z.values).ravel()
    return resid

def fittingSimpleGrowth(cases, IC, inits, bounds, timevect, flag, method='trf'):
    '''
    Fitting model by Least Square curve fitting - minimize L2-norm
    
    Parameters
    ----------
    cases: vector of incidence
    timevect: vector of time points
    method: least_squares fitting method
    
    Returns
    -------
    Ptrue: a vector of estimated parameters after model fitting
    '''
    
    P = least_squares(ResidFun, inits, bounds=bounds,method=method, ftol=1e-15, xtol=1e-15, gtol=1e-15, max_nfev=1000000, args=(IC, timevect, flag, cases))
    Ptrue = P["x"]
    return Ptrue
#############################################################################################################
##################SUB-EPIDEMICS MODEL########################################################################


def numberSubepidemics(K0,C_thr,q):

    if q==0:
        n=18
    else:
        n = math.floor(-(1/q)*math.log(C_thr/K0)+1)
    return n

def modifiedLogisticGrowthPatch(x,t,params, npatches,onset_thr):
    r, p, a, q, K = params
    
    global invasions
    
    dx=np.zeros(npatches)
    for j in range(npatches):
        if invasions[j]==0:
            if x[j-1]>=onset_thr:
                invasions[j]= 1 
            
        K1=K*math.exp(-q*(j-1)); 
        dx[j]=invasions[j]*(r*(x[j]**p)*(1-(x[j]/K1)**a))
    return dx


def solvedModifiedLogisticGrowthPatch(params,I0,tf,npatch,onset_thr):
    r, p, a, q, K = params
    global invasions
    invasions = np.zeros(npatch)
    invasions[0] = 1
    if onset_thr>K:
        npatch=1
    else:
        npatches2=numberSubepidemics(K,onset_thr,q)
    
        npatches2=max(npatches2,1)
    
        npatch=min(npatch,npatches2)


    IC=np.ones(int(npatch))

    IC[0]=I0
    
    x = odeint(modifiedLogisticGrowthPatch, IC, tf, args=(params, npatch,onset_thr))

    y=np.sum(x, axis=1)
    totinc= np.diff(y)
    totinc = np.insert(totinc, 0, totinc[0]-(npatch-1))
    return totinc

def ResidFun2(params, I0,tf,npatch,onset_thr, cases):
    y = solvedModifiedLogisticGrowthPatch(params,I0,tf,npatch,onset_thr)
    resid = y - np.array(cases)
    return resid

def fittingModifiedLogisticGrowth(cases, I0, inits, bounds, tf, npatch, onset_thr, method='trf'):
    P = least_squares(ResidFun2, inits, bounds=bounds,method=method, ftol=1e-15, xtol=1e-15, gtol=1e-15, max_nfev=1000000, args=(I0, tf, npatch, onset_thr, cases))
    return P["x"]

def fittingModifiedLogisticGrowthPatch(cases,inits, bounds, tf, method='trf'):
    
    I0= cases[0]
    peak_ind = np.argmax(cases)
    
    cum_at_peak = sum(np.array(cases)[:(peak_ind+1)])
    
    cum_to_peak = np.unique(np.cumsum(cases[:(peak_ind+1)]))
    
    
    onset_thrs = cum_to_peak[cum_to_peak <= 0.8*cum_at_peak]
    npatches = range(1, int(min(18, math.ceil(tf[-1]/14 + 1)))+1)
    rmse = None
    for npatch in npatches:
        for onset_thr in onset_thrs:
            P = least_squares(ResidFun2, inits, bounds=bounds,method=method, ftol=1e-15, xtol=1e-15, gtol=1e-15, max_nfev=1000000, args=(I0, tf, npatch, onset_thr, cases))
            if rmse is None:
                rmse = P["cost"]
                Ptrue = P["x"]
                npatch_est, onset_thr_est = npatch, onset_thr
            else:
                if P["cost"] < rmse:
                    rmse = P["cost"]
                    Ptrue = P["x"]
                    npatch_est, onset_thr_est = npatch, onset_thr
    return [Ptrue, npatch_est, onset_thr_est]

