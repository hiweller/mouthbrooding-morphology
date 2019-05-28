# Chapter 3: Introduction to Brownian Motion

## 3.2: Properties of Brownian Motion

* Brownian motion is the workhorse "random walk" model of evolution. It's an implementation of the assumption that a trait will change randomly, both in direction and distance, over any time interval.

* The core of this assumption is that you have an output (trait) whose motion (evolution) is determined by a bunch of small, more or less random forces, rather than a dominate, directional pressure.

* But apparently it's not equivalent to genetic drift?
  
  * Evolutionary model using Brownian motion usually models the mean character value, $\bar z$, of a population. At a given time, the mean trait value will be $\bar z(t)$. 

* You only need two parameters for a BMM: starting trait value, $\bar z(0)$, and the evolutionary rate parameter, $\sigma^2$, which determines how fast traits walk.

* If a trait is changing under a BMM, its values over an interval of time are drawn from a normal distribution with mean 0 and variance proportional to evolutionary rate x time (variance = $\sigma^2t$), making them easy to simulate.

* So the more time has passed, the wider the variation in the trait value.

* If you simulate BM enough times, then sample the distribution of the trait at given timepoints, you'll see a normal distribution centered around the starting value of the trait that gets wider as more time passes.

### 3 main properties of Brownian Motion

1. $E[\bar z(t)] = \bar z(0)$
2. Each successive interval of the "walk" is independent
3. $\bar z(t) \sim N(\bar z(0),\sigma^2t)$

* Ok he just listed these out but I don't know what the hell these mean.

1) $E[\bar z(t)] = \bar z(0)$: The *expected value of the character* (E) at time $t$ is equal to the *starting value of the character*. This obviously won't usually be the case -- the trait value will wander as a function of the normal distribution -- but we don't expect it to drift toward any other value in particular. So on average, it will end up right where it started.

2) 
