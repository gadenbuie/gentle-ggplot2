FROM rocker/verse
LABEL maintainer="gadenbuie"
RUN git clone https://github.com/gadenbuie/trug-ggplot2.git
RUN ["install2.r", "skimr", "babynames"]
RUN ["installGithub.r", "yihui/xaringan@646f04e"]
