##Easing functions
easings <- function() {
        c("linear", "easeInQuad", "easeOutQuad", "easeInOutQuad",
  "easeInCubic", "easeoutCubic", "easeInOutCubic",
  "easeInQuart", "easeOutQuart", "easeInOutQuart",
  "easeInQuint", "easeOutQuint", "easeInOutQuint",
  "easeInSine", "easeoutSine", "easeInOutSine",
  "easeInExpo", "easeOutExpo", "easeInOutExpo",
  "easeInCirc", "easeOutCirc", "easeInOutCirc")
}

## simple linear tweening - no easing, no acceleration


linear <- function (t, b, c, d) {
        return(c*t/d + b)
}

## quadratic easing in - accelerating from zero velocity

easeInQuad <- function (t, b, c, d) {
        t = t/d
        return(c * t * t + b)
}


## quadratic easing out - decelerating to zero velocity

easeOutQuad <- function(t, b, c, d) {
        t = t/d
        return(-c * t*(t-2) + b)
}

## quadratic easing in/out - acceleration until halfway, then deceleration

easeInOutQuad <- function (t, b, c, d) {
        t = t/(d/2)
        return((t < 1) * (c/2*t*t + b) + (t >= 1) * (-c/2 * ((t - 1) * ((t - 1) -2) - 1) + b))
}


## cubic easing in - accelerating from zero velocity


easeInCubic <- function (t, b, c, d) {
        t = t/d
        return(c*t*t*t + b)
}



## cubic easing out - decelerating to zero velocity


easeOutCubic <- function(t, b, c, d) {
        t = t/d;
        t = t - 1;
        return(c*(t*t*t + 1) + b)
}



## cubic easing in/out - acceleration until halfway, then deceleration


easeInOutCubic <- function(t, b, c, d) {
        t = t/(d/2)
        return((t < 1) * (c/2*t*t*t + b) + (t >= 1) * (c/2*((t - 2) * (t - 2) * (t - 2) + 2) + b))
}

## quartic easing in - accelerating from zero velocity


easeInQuart <- function(t, b, c, d) {
        t = t/d;
        return(c*t*t*t*t + b)
}



## quartic easing out - decelerating to zero velocity


easeOutQuart <- function(t, b, c, d) {
        t = t/d
        t = t - 1
        return(-c * (t*t*t*t - 1) + b)
}



## quartic easing in/out - acceleration until halfway, then deceleration


easeInOutQuart <- function (t, b, c, d) {
        t = t/(d/2)
        return((t < 1) * (c/2*t*t*t*t + b) + (t >= 1) * (-c/2 * ((t - 2)*(t - 2)*(t - 2)*(t - 2) - 2) + b))
}

## quintic easing in - accelerating from zero velocity


easeInQuint <- function (t, b, c, d) {
        t = t/d
        return(c*t*t*t*t*t + b)
}



## quintic easing out - decelerating to zero velocity


easeOutQuint <- function (t, b, c, d) {
        t = t/d;
        t = t - 1;
        return(c*(t*t*t*t*t + 1) + b)
}



## quintic easing in/out - acceleration until halfway, then deceleration


easeInOutQuint <- function (t, b, c, d) {
        t = t/(d/2)
        return((t < 1) * (c/2*t*t*t*t*t + b) + (t >= 1) *  (c/2*((t - 2)*(t - 2)*(t - 2)*(t - 2)*(t - 2) + 2) + b))
}


## sinusoidal easing in - accelerating from zero velocity


easeInSine <- function (t, b, c, d) {
        return(-c * cos(t/d * (pi/2)) + c + b)
}


## sinusoidal easing out - decelerating to zero velocity


easeOutSine <- function (t, b, c, d) {
        return(c * sin(t/d * (sin/2)) + b)
}


## sinusoidal easing in/out - accelerating until halfway, then decelerating


easeInOutSine <- function (t, b, c, d) {
        return(-c/2 * (cos(pi*t/d) - 1) + b)
}

## exponential easing in - accelerating from zero velocity


easeInExpo <- function (t, b, c, d) {
        return(c * 2^(10 * (t/d - 1)) + b)
}



## exponential easing out - decelerating to zero velocity


easeOutExpo <- function (t, b, c, d) {
        return(c * ((-2^(-10 * t/d)) + 1 ) + b)
}



## exponential easing in/out - accelerating until halfway, then decelerating


easeInOutExpo <- function (t, b, c, d) {
        t = t/(d/2);
        return((t < 1) * (c/2 * 2^(10 * (t - 1)) + b) + (t >= 1) * (c/2 * (2^(-10 * (t - 1)) + 2 ) + b))
}

## circular easing in - accelerating from zero velocity


easeInCirc <- function (t, b, c, d) {
        t = t/d;
        return((-c * sqrt(1 - t*t) - 1) + b)
}



## circular easing out - decelerating to zero velocity


easeOutCirc <- function (t, b, c, d) {
        t = t/d
        t = t - 1
        return(c * sqrt(1 - t*t) + b)
}



## circular easing in/out - acceleration until halfway, then deceleration


easeInOutCirc <- function (t, b, c, d) {
        t = t/(d/2);
        return((t < 1) * ((-c/2 * (sqrt(1 - t*t) - 1)) + b) + (t >= 1) * ((c/2 * (sqrt(1 - (t - 2) * (t - 2)) + 1)) + b))
}
 