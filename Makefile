
.PHONY: ui docker build stop run deploy
APPNAME=htest
TARGETHOST=beta
#BUILDX_NOCACHE=--no-cache
#BUILDX_PLAIN=--progress plain

LOG_TO_SPLUNK=--log-driver=splunk \
              	--log-opt splunk-token=5962ec58-5c10-44c8-ac7a-3cfc0bdf52e6 \
              	--log-opt splunk-url=http://ceres.lan:8088 \
              	--log-opt splunk-format=json

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
	--privileged \
	$(LOG_TO_SPLUNK) \
	-p 8080:8080 \
	--name $(APPNAME) --hostname \$$HOST $(APPNAME):latest"

#	--log-driver=journald \
#	--log-opt tag=$(APPNAME) \

logs:
	@ssh $(TARGETHOST) "docker logs -f $(APPNAME)"

attach:
	@echo " => connecting..."
	@ssh -t $(TARGETHOST) "docker exec -it $(APPNAME) \
	/opt/app/bin/app remote_console"


splunk:
	@-ssh $(TARGETHOST) "docker kill splunkuf" &> /dev/null || true
	@-ssh $(TARGETHOST) "docker rm splunkuf" &> /dev/null || true
	@ssh $(TARGETHOST) "docker run -d --restart unless-stopped \
	-p 9997:9997 \
	-e 'SPLUNK_START_ARGS=--accept-license' \
	-e 'SPLUNK_USERNAME=username' \
	-e 'SPLUNK_PASSWORD=password' \
	--hostname \$$HOST \
	--name splunkuf \
	splunk/universalforwarder:latest"

