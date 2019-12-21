# syntax = docker/dockerfile:experimental
# ^^^^
# this line must be here and "features" must be set in docker json file:
#		{
#		  "debug" : true,
#		  "experimental" : true,
#		  "features":{"buildkit": true}    <-- this must be set
#		}
#
# build with
# docker buildx build -o type=docker --target runner -t APP:latest .

FROM erlang:alpine as builder
WORKDIR /app

# Install git for fetching non-hex depenencies.
# Add any other Alpine libraries needed to compile the project here.
# See https://wiki.alpinelinux.org/wiki/Local_APK_cache for details
# on the local cache and need for the symlink
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
		ln -s /var/cache/apk /etc/apk/cache && \
    apk add --update git tree make build-base
#
## build and cache dependencies as their own layer
COPY rebar.config rebar.lock ./
COPY _checkouts ./_checkouts
RUN --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
	rebar3 compile

#
## build and cache dependencies as their own layer as prod
FROM builder as prod_compiled
RUN --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 as prod compile

#
## build the app source
FROM prod_compiled as releaser
COPY . /app
RUN --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    mkdir -p /opt/rel && \
#    for f in $(find /app/_build/prod/rel -type l);do cp --remove-destination $(readlink $f) $f; echo "unsymlinked $f"; done && \
    ls -laR /app/_build && \
    rebar3 compile && \
#		rebar3 as prod tar && \
#		ls -laR /app/_build && \
		tar -zxvf /app/_build/default/rel/*/*.tar.gz -C /opt/rel && \
#		ls -laR /app/_build
		ls -laR /opt/rel

# runner
FROM alpine as runner
# openssl needed by the crypto app
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
		ln -s /var/cache/apk /etc/apk/cache && \
    mkdir /lib64 && \
		ln -s /lib/libc.musl-x86_64.so.1 /lib64/ld-linux-x86-64.so.2 && \
    apk add --no-cache --update ca-certificates openssl ncurses

WORKDIR /opt/app
COPY --from=releaser /opt/rel .

# copy the ui
#COPY ui/dist/ ui/dist/
EXPOSE 8080
# execute the binary when the container is entered
ENTRYPOINT ["/opt/app/bin/app"]
CMD ["foreground"]
