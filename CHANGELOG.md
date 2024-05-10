# Change Log

All notable changes to this project will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.


## 0.1.2 (Unreleased)
> Released N/A

* Add variable for inhibit display function (ce5a1620d0448e0a0959a83f27721246ab78fa0e)
* Add variable to delay sideline renderer (638fb873d6071eadac798b92dd212889df45f7e3)
* Don't color face when there is text property (629a21184552687f5b2ea7a378f0c363f0bd379a)
* Accept cons cell for `backends` variables (1f628103d24c8871396da1405915b5104c26a203)
* Calculate string length in pixel instead (bf375ac2b659e0312bc5a3a67aded4d3569c0b51)
* Add capability to display backend name (#3)
* Don't load `shr` if not needed (f7e00ca0d445d0395a4912fb6d30e4d46ba7e87c)
* Fix alignment issue when text-scale amount is not zero (8a981bc41f0169d2b6553988bd091d4eaf5bd134)
* Upgrade rendering with opposing length (#11)
* Better construct backend name (b63cea69948d386f007004ba6a272fc0e944ffb9)
* feat: Categorize overlays by backend name (0d63da5246c49e3059680ebb02f64a2c3bb914e8)
* fix: Compatible with buffer face font (a4626181434534385fbb199fa17d606f89e86e7c)

## 0.1.1
> Released Jun 15, 2022

* Add new variables `pre/post-render-hook` (dfbe93b3628daac9b7e995ef8e0b712772169aee)
* Add new variable `reset-hook` (dfbe93b3628daac9b7e995ef8e0b712772169aee)
* Fix edge cases (dfbe93b3628daac9b7e995ef8e0b712772169aee)
* Fix freezes on both ends (ff2240d515149fac172381742ec5367f0cc8ec14)
* feat(skip): split `skip-current-line` to 2 variables, `left` and `right` (fcae8923e355892339bfd4c4c228945127eb22a4)

## 0.1.0
> Released Jun 14, 2022

* Initial release
