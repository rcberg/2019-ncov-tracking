---
title: A Basic Epidemiological Contagion Model
mathjax: true
---


Epidemiologists use mathematical models to understand how diseases spread throughout a population. [Dr. James Stock](https://drive.google.com/file/d/1Vu0wl-9K2dh8MpMqaO85MvE6UH7gcRLx/view) offers a simplified model based on more complicated epi. models, which I have been inspired to simulate. 

- To check out the simulation widget, [click here](https://rcberg.shinyapps.io/econepimodel/).

- For some helpful background, continue to read-on.

## Background

Epidemiologists model disease spread by looking at susceptible individuals, the infection rate, and the recovery rate. Populations start out with individuals susceptible to disease, and given an initial infected individual (*vector*) the disease spreads. As the population contracts the disease, they are classified as no-longer-susceptible because they will suffer from the disease, and then *recover* to a point of immunity where they aren't susceptible to the illness anymore.

### Quick note on deaths

This is a very simple model that just attempts to describe spread dynamics. There is no death-- individuals get the disease and eventually recover. Epidemiologists have done a better job than me of adding things like mortality, but  I might try and add that in the future at some point after reading the research.

## Simulation equations

The disease is described by a system of three equations at some given point in time $t$; one describing how the *susceptible* population changes, another describing how the *recovered* population changes, and lastly (but not least!) how the *infected* population changes.

$$
\Delta S_t = -\beta * I_{t-1}\frac{S_{t-1}}{N}
$$

$$
\Delta R_t = \gamma * I_{t-1}
$$

$$
\Delta I_t= \beta * I_{t-1}\frac{S_{t-1}}{N} - \gamma * I_{t-1}
$$

## Explanation 

Here $\beta$ is the *population transmission rate*, reflecting the rate which each infected person contaminates others each period. This is what policies like shelter-in-place orders and social distancing reduce. Reducing it is the main way that we respond to disease outbreaks. On the other hand $\gamma$ describes the rate at which infected persons recover.

The disease spreads at first because there are many susceptible individuals and few infected individuals. As the disease spreads, the population which is susceptible goes down (because they are either sick or recovered and thus immune), and infections eventually slow. The system settles to an equilibrium where the disease runs its course.