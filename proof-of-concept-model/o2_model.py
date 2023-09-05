#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#Created on Tue Mar  7 10:15:01 2023
#@author: Robert Ladwig
# Toy model to explore the hypothesis that elevation-induced changes in
# initial conc. of DO are sufficient to affect bottom hypoxia

import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from math import pi, exp, sqrt, log, atan, sin, radians, nan

## function to calculate density from temperature
def calc_dens(wtemp):
    dens = (999.842594 + (6.793952 * 1e-2 * wtemp) - (9.095290 * 1e-3 *wtemp**2) +
      (1.001685 * 1e-4 * wtemp**3) - (1.120083 * 1e-6* wtemp**4) + 
      (6.536336 * 1e-9 * wtemp**5))
    return dens

## this is our attempt for turbulence closure, estimating eddy diffusivity
def eddy_diffusivity(rho, depth, g, rho_0, area):
    km = 1.4 * 10**(-7)
    
    rho = np.array(rho)
    
    buoy = np.ones(len(depth)) * 7e-5
    buoy[:-1] = np.abs(rho[1:] - rho[:-1]) / (depth[1:] - depth[:-1]) * g / rho_0
    buoy[-1] = buoy[-2]
        
    low_values_flags = buoy < 7e-5  # Where values are low
    buoy[low_values_flags] = 7e-5
    
    ak = 0.00706 * area[0]**0.56
    kz = ak * (buoy)**(-0.43)
    
    return(kz + km)

zmax = 10
dx = 0.5
dt = 1

elevation = 4000#4000#000
tempchange = 0#1.5#1.5#1.5#1.5#1

area = np.linspace(1e4,1e1, np.int(zmax/ dx)) # m2
depth = np.linspace(0.1, zmax, np.int(zmax/dx)) # m
active_area = np.linspace(1e-2,1.0, np.int(zmax/ dx)) # -

temp = np.ones(np.int(zmax / dx)) * 10  + tempchange # deg C
o2_initial = np.ones(np.int(zmax / dx)) * (14.7-(0.0017*elevation))*exp(-0.0225*np.mean(temp)) # g/m3

temp_dep = (-139.34411 + (1.575701 * 10**5)/(temp + 273.15) -  (6.642308*10**7) / (temp +273.15)**2 +
                                         (1.243800*10**10)/(temp + 273.15)**3 - (8.621949*10**11)/(temp + 273.15)**4)
height_dep = np.ones(np.int(zmax / dx)) * (1 - 0.11988 * elevation/1000 + 6.10834 * 10**(-3) * (elevation/1000)**2 -
                                           1.60747 *10**(-4) * (elevation/1000)**3)
o2_initial = height_dep * np.exp(temp_dep)

vol_flux = -0.01 # g/m3/d
area_flux = - 0.001 # g/m2/d
benthic_thickness = 0.001  # m
benthic_diff = 10**(-4.410 + 773.8/(temp[-1] + 273.15) - ((506.4)/(temp[-1] + 273.15))**2) / 10**4 * 86400 # m2/d
arrhenius = 1.08 # -

maxt = 60 # days
times = np.arange(0, maxt, dt)

o2_res = np.full([np.int(zmax/dx), maxt], np.nan)

for idn, n in enumerate(times):
    
    if (idn == 0):
        un = o2_initial
        u = un
    
    u = un + (vol_flux + (area_flux - benthic_diff / benthic_thickness * un) * area/(area * dx) * active_area) * dt * arrhenius**(temp -20)

    u[u < 0] = 0
    un = u
    
    kzn = eddy_diffusivity(rho = calc_dens(temp), depth = depth, g = 9.81, rho_0 = np.mean(calc_dens(temp)), area = area) # m2/d
    
    
    j = len(un)
    y = np.zeros((len(un), len(un)))

    alpha = (area * kzn * dt) / (2 * dx**2)
        
    az = - alpha # subdiagonal
    bz = (area + 2 * alpha) # diagonal
    cz = - alpha # superdiagonal
        
    bz[0] = 1
    bz[len(bz)-1] = 1
    cz[0] = 0
        
    az =  np.delete(az,0)
    cz =  np.delete(cz,len(cz)-1)
        
        # tridiagonal matrix
    for k in range(j-1):
        y[k][k] = bz[k]
        y[k][k+1] = cz[k]
        y[k+1][k] = az[k]
        

    y[j-1, j-2] = 0
    y[j-1, j-1] = 1


    mn = un * 0.0    
    mn[0] = un[0]
    mn[-1] = un[-1]
        
    for k in range(1,j-1):
        mn[k] = alpha[k] * un[k-1] + (area[k] - 2 * alpha[k]) * un[k] + alpha[k] * un[k+1]

    u = np.linalg.solve(y, mn)
    
    o2_res[:, idn] = u

# heatmap of o2  
plt.subplots(figsize=(140,80))
sns.heatmap(o2_res, cmap=plt.cm.get_cmap('Spectral_r'), xticklabels=1000, yticklabels=2)
plt.show()

fig=plt.figure(figsize=(6,4))
plt.plot(o2_res[0,:], color="black", label = 'surface')
plt.plot(o2_res[-1,:],color="red", label = 'bottom')
plt.legend(loc="upper right")
plt.title("Near-sediment DO conc.")
plt.xlabel("Time (days)")
plt.ylabel("DO conc. (mg/L)")
plt.show()

times[default <=2][0]
times[warming <=2][0]
times[mountain <=2][0]
times[mountainwarming <=2][0]

plt.rcParams.update({'font.size': 22})
fig=plt.figure(figsize=(10,8))
plt.plot(default, color="black", label = 'Default')
plt.plot(warming,color="red", label = 'Warming')
plt.plot(mountain,color="green", label = 'Elevation')
plt.plot(mountainwarming,color="blue", label = 'Warming+Elevation')
plt.axhline(y=2, xmin=0, xmax=60, color= 'gray',  linestyle='dotted')
plt.axvline(x=15-0.6, ymin=0, ymax=2, color= 'black',  linestyle='dotted')
plt.axvline(x=12-0.4, ymin=0, ymax=2, color= 'red',  linestyle='dotted')
plt.axvline(x=10-0.3, ymin=0, ymax=2, color= 'green',  linestyle='dotted')
plt.axvline(x=8-0.2, ymin=0, ymax=2, color= 'blue',  linestyle='dotted')
plt.legend(loc="upper right")
plt.title("Near-sediment DO conc.")
plt.xlabel("Time (days)")
plt.ylabel("DO conc. (mg/L)")
plt.show()



