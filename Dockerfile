FROM rocker/verse
LABEL maintainer="gadenbuie"
#RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
# && apt-get install -y git-core \
#	libxml2-dev \
#	make \
#	pandoc \
#	pandoc-citeproc
RUN ["install2.r", "skimr", "babynames"]
RUN ["installGithub.r", "yihui/xaringan@646f04e"]
