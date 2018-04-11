FROM ubuntu:latest
RUN apt-get update
RUN apt-get -y install pkg-config
RUN apt-get -y install r-recommended
RUN apt-get -y install texlive-full
RUN apt-get -y install make
RUN apt-get -y install libcurl4-gnutls-dev

RUN apt-get -y install build-essential
RUN apt-get -y install libxml2-dev
RUN apt-get -y install libssl-dev
RUN apt-get -y install libnlopt-dev

#setup R configs
RUN echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.us.r-project.org'; options(repos = r);" > ~/.Rprofile

RUN Rscript -e 'install.packages("devtools")'
RUN Rscript -e 'install.packages("lme4")'
RUN Rscript -e 'install.packages("ggplot2")'

RUN mkdir mturk_search_behavior
COPY . mturk_search_behavior/ 
RUN cd mturk_search_behavior/writeup && make mturk_search_behavior.pdf
