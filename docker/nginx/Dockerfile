FROM nginx:1.11

RUN rm /etc/nginx/conf.d/default.conf

ADD ./devops/nginx/kalesville.nginx.conf /etc/nginx/conf.d/
ADD ./devops/nginx/nginx.conf /etc/nginx/nginx.conf
ADD ./static/ /usr/share/nginx/static
