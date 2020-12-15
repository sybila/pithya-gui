[![Build Status](https://travis-ci.org/sybila/pithya-core.svg?branch=master)](https://travis-ci.org/sybila/pithya-core)
[![License](https://img.shields.io/badge/License-GPL%20v3-blue.svg?style=flat)](https://github.com/sybila/biodivine-ctl/blob/master/LICENSE.txt)

Pithya is a tool for parameter synthesis of ODE-based models and properties based on a hybrid extension of CTL.

## Online demo

To try Pithya online, visit [pithya.fi.muni.cz](https://pithya.fi.muni.cz/). In case of any problems/questions, feel free to contact us at [sybila@fi.muni.cz](mailto:sybila@fi.muni.cz).

## Installation and dependencies

To run Pithya, you need to have **Java 8+** and Microsoft **Z3 4.5.0** 
and **R** installed. If your OS is supported, we strongly recommend downloading precompiled
Z3 binaries from github (Pithya allows you to specify a custom Z3 
location, so you don't necessarily need to have it in your PATH, but we recommend doing that anyway).

The full installation process for the GUI is described in the [manual](http://biodivine.fi.muni.cz/docs/pithya/manual.pdf).
You can also use Pithya from command line. For more information, see the [core module](https://github.com/sybila/pithya-core) repository.

### Project status

Pithya is composed of several independent modules. Here you can find links to them and their current status:

[![Release](https://jitpack.io/v/sybila/ctl-model-checker.svg)](https://jitpack.io/#sybila/ctl-model-checker)
[![Build Status](https://travis-ci.org/sybila/ctl-model-checker.svg?branch=master)](https://travis-ci.org/sybila/ctl-model-checker)
[![codecov.io](https://codecov.io/github/sybila/ctl-model-checker/coverage.svg?branch=master)](https://codecov.io/github/sybila/ctl-model-checker?branch=master)
[CTL Model Checker](https://github.com/sybila/ctl-model-checker)

[![Release](https://jitpack.io/v/sybila/huctl.svg)](https://jitpack.io/#sybila/huctl)
[![Build Status](https://travis-ci.org/sybila/huctl.svg?branch=master)](https://travis-ci.org/sybila/huctl)
[![codecov.io](https://codecov.io/github/sybila/huctl/coverage.svg?branch=master)](https://codecov.io/github/sybila/huctl?branch=master)
[CTL Query Parser](https://github.com/sybila/huctl)

[![Release](https://jitpack.io/v/sybila/ode-generator.svg)](https://jitpack.io/#sybila/ode-generator)
[![Build Status](https://travis-ci.org/sybila/ode-generator.svg?branch=master)](https://travis-ci.org/sybila/ode-generator)
[![codecov.io](https://codecov.io/github/sybila/ode-generator/coverage.svg?branch=master)](https://codecov.io/github/sybila/ode-generator?branch=master)
[ODE State Space Generator](https://github.com/sybila/ode-generator)
