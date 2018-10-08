<!-- README.md is generated from README.Rmd. Please edit that file -->
ReproducibleR
=============

The goal of ReproducibleR is to learn how to use R to make your analysis
reproducible.

Prerequisites
-------------

You need to have the following softwares: -
[Git](https://git-scm.com/downloads) to version your work -
[Docker](https://docs.docker.com/install/) to ensure that we work in the
same container

### Install Docker

-   [For windows](https://docs.docker.com/docker-for-windows/install/)
-   [For Linux](https://docs.docker.com/docker-for-windows/install/)
-   [Mac OS](https://docs.docker.com/docker-for-mac/install/)

Once you have docker, please run the following command. You will get a
container with R, RStudio and several packages that we will use. See
more on [Rocker](https://github.com/rocker-org/rocker).

``` bash
docker run --rm -p 8787:8787 rocker/verse
```

Once you have downloaded it, you can open an Rstudio session in your web
browser. On Linux system, it will be at <http://localhost:8787/>. On
other systems, `localhost` will be replaced by your ip address. Your
will find it when you launch the Docker Quickstart Terminal. Please see
[this
tutorial](http://ropenscilabs.github.io/r-docker-tutorial/02-Launching-Docker.html).

``` r
## basic example code
```

Documentation
-------------

-   Using Docker with R: [Ropenscilabs
    course](http://ropenscilabs.github.io/r-docker-tutorial/)
-   R compodium: [Example from Carl
    Boettiger](https://github.com/cboettig/template)
