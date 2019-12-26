
.PHONY: ui docker build stop run deploy
APPNAME=htest
TARGETHOST=beta
#BUILDX_NOCACHE=--no-cache
#BUILDX_PLAIN=--progress plain

LOG_TO_SPLUNK=--log-driver=splunk \
              	--log-opt splunk-token=5962ec58-5c10-44c8-ac7a-3cfc0bdf52e6 \
              	--log-opt splunk-url=http://ceres.lan:8088 \
              	--log-opt splunk-format=json \
              	--log-opt tag=$(APPNAME)

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
	$(LOG_TO_SPLUNK) \
	--privileged \
	-p 8080:8080 \
	--name $(APPNAME) --hostname \$$HOST $(APPNAME):latest"


logs:
	@ssh $(TARGETHOST) "docker logs -f $(APPNAME)"

attach:
	@echo " => connecting..."
	@ssh -t $(TARGETHOST) "docker exec -it $(APPNAME) \
	/opt/app/bin/app remote_console"


splunk:
	@-ssh $(TARGETHOST) "docker kill splunkuf" &> /dev/null || true
	@-ssh $(TARGETHOST) "docker rm splunkuf" &> /dev/null || true
	@ssh $(TARGETHOST) "docker run -d \
	-u root \
	-p 9997:9997 \
	-e 'SPLUNK_START_ARGS=--accept-license' \
	-e 'SPLUNK_USER=root' \
	-e 'SPLUNK_USERNAME=admin' \
	-e 'SPLUNK_PASSWORD=changeme' \
	-v /var/lib/docker/containers:/var/lib/docker/containers \
	--restart unless-stopped \
	--hostname \$$HOST \
	--name splunkuf \
	splunk/universalforwarder:latest"
	sleep 3; echo ".";sleep 3; echo ".";sleep 3; echo ".";sleep 3; echo ".";sleep 3; echo ".";sleep 3; echo "."
	ssh $(TARGETHOST) "docker exec -e 'SPLUNK_START_ARGS=--accept-license' -u root splunkuf /bin/bash -c '/opt/splunkforwarder/bin/splunk add forward-server ceres.lan:9997 -auth admin:changeme'"
	ssh $(TARGETHOST) "docker exec -e 'SPLUNK_START_ARGS=--accept-license' -u root splunkuf /bin/bash -c '/opt/splunkforwarder/bin/splunk set deploy-poll ceres.lan:8089 -auth admin:changeme'"
	ssh $(TARGETHOST) "docker exec -e 'SPLUNK_START_ARGS=--accept-license' -u root splunkuf /bin/bash -c '/opt/splunkforwarder/bin/splunk add monitor -source \"/var/lib/docker/containers/*/*json.log\" -sourcetype \"json:log\" -auth admin:changeme'"
	ssh $(TARGETHOST) "docker restart splunkuf"

