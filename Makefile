
.PHONY: ui docker build stop run deploy
APPNAME=htest
TARGETHOST=beta
#BUILDX_NOCACHE=--no-cache
#BUILDX_PLAIN=--progress plain

ui:
	@-rm -rf priv/ui
	@cp -a ../vui/dist priv/ui

docker: ui
	@docker buildx build $(BUILDX_NOCACHE) $(BUILDX_PLAIN) -o type=docker  \
	--target runner -t $(APPNAME):latest .

#run:
#	@docker run -it --rm $(APPNAME):latest console

stop:
	@echo " => stopping..."
	@-ssh $(TARGETHOST) "docker kill $(APPNAME)" &> /dev/null || true
	@-ssh $(TARGETHOST) "docker rm $(APPNAME)" &> /dev/null || true

deploy: docker
	@echo " => tarring..."
	@docker save -o /tmp/$(APPNAME).tar $(APPNAME):latest &> /dev/null || true
	@echo " => pushing..."
	@rsync -avzC /tmp/$(APPNAME).tar $(TARGETHOST):. &> /dev/null || true
	@echo " => loading..."
	@ssh $(TARGETHOST) "docker load -i $(APPNAME).tar" &> /dev/null || true
	@echo " => ready."

run: stop
	@echo " => running..."
	@ssh $(TARGETHOST) "docker run -d --restart unless-stopped \
	--device=/dev/ttyACM0 \
	--privileged --log-driver=journald --log-opt tag=$(APPNAME) -p 8080:8080 \
	--name $(APPNAME) --hostname \$$HOST $(APPNAME):latest"

logs:
	@ssh $(TARGETHOST) "docker logs -f $(APPNAME)"

attach:
	@echo " => connecting..."
	@ssh -t $(TARGETHOST) "docker exec -it $(APPNAME) \
	/opt/app/bin/app remote_console"