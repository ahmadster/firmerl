FROM erlang:alpine as builder
WORKDIR /app

# Install git for fetching non-hex depenencies.
# Add any other Alpine libraries needed to compile the project here.
# See https://wiki.alpinelinux.org/wiki/Local_APK_cache for details
# on the local cache and need for the symlink
RUN ln -s /var/cache/apk /etc/apk/cache && \
    apk add --update git tree make build-base
ADD . /app
RUN mkdir -p /opt/rel && \
	rebar3 as prod tar && \
	tar -zxf /app/_build/prod/rel/*/*.tar.gz -C /opt/rel && \
	mv -f /app/_build/default/lib/* /opt/rel/lib/.

# runner
FROM alpine as runner
# openssl needed by the crypto app
RUN ln -s /var/cache/apk /etc/apk/cache && \
    mkdir /lib64 && \
		ln -s /lib/libc.musl-x86_64.so.1 /lib64/ld-linux-x86-64.so.2 && \
		echo "alias ll='ls -laH'" > /root/.zshrc && \
		echo 'export PROMPT="%F{blue}%~%f %F{green}%#%f "' >> /root/.zshrc && \
		cp /root/.zshrc /root/.profile && \
    apk add --no-cache --update ca-certificates openssl ncurses tree zsh httpie

WORKDIR /app
COPY --from=builder /opt/rel .

# copy the ui
#COPY ui/dist/ ui/dist/
EXPOSE 8080
# execute the binary when the container is entered
ENTRYPOINT ["/app/bin/app"]
CMD ["console"]
