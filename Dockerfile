FROM openanalytics/r-base

# required R libraries
RUN R -e "install.packages(c('shiny', 'plyr', 'ggplot2', 'reshape2', 'RColorBrewer', 'tester'), repos='https://cloud.r-project.org/')"

# copy app to image
RUN mkdir /root/krano
COPY src /root/krano

EXPOSE 80

CMD ["R", "-e", "shiny::runApp('/root/krano', port=80, host='0.0.0.0')"]