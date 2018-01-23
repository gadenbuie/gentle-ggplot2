# A Gentle Guide to the Grammar of Graphics with `ggplot2`

These are the slides and repo for my talk at the Tampa R Users Meetup on January 23, 2018.

The slides are available here: <https://gadenbuie.github.io/trug-ggplot2>


## Follow Along

You can use the script [`0-companion-script.R`](0-companion-script.R) to follow along during the talk or to review the plots later.

## Docker

If you happen to use [Docker], you can use the included `Dockerfile` to build a Docker container with everything you need (including RStudio thanks to the [Rocker] project).
Just clone this repo and run

```bash
# Build container image
docker build -t trug-ggplot2 ./

# Run image
docker run -d -p 8787:8787 trug-ggplot2
```

Then point your browser at `127.0.0.1:8787` and login with `rstudio` (for both).
The RStudio project for these slides will be under `trug-ggplot2/`


[Docker]: https://docs.docker.com/installation/
[Rocker]: https://github.com/rocker-org/rocker
