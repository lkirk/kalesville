FROM nginx:1.11

RUN rm /etc/nginx/conf.d/default.conf

ADD ./docker/nginx/kalesville.nginx.conf /etc/nginx/conf.d/
ADD ./docker/nginx/nginx.conf /etc/nginx/nginx.conf
ADD ./static/ /usr/share/nginx/static
