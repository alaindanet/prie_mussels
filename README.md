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

#### Pointing a Docker session to your project

``` bash
docker run --rm -p 8787:8787 -v /path_to_your_project/project_directory:/home/rstudio/project_directory rocker/verse
```

#### Commit inside a docker container

While running R in a docker container, git will not know you since you
are not in your system environment.

If you run your Docker session which points to a git repository, you can
configure it locally. Your local git config will be available in the
Docker environment.

In your project root, you can run:

``` bash
git config user.name "FirstName LastName"
git config user.email "MY_NAME@example.com"
```

You can check the local set up: `cat .git/config`

Documentation
-------------

-   Using Docker with R: [Ropenscilabs
    course](http://ropenscilabs.github.io/r-docker-tutorial/)
-   R compodium: [Example from Carl
    Boettiger](https://github.com/cboettig/template)