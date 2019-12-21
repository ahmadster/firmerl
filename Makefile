
.PHONY: docker build run deploy
APPNAME=htest
TARGETHOST=beta
#BUILDX_NOCACHE=--no-cache
#BUILDX_PLAIN=--progress plain

docker:
	@docker buildx build $(BUILDX_NOCACHE) $(BUILDX_PLAIN) -o type=docker --target runner -t htest:latest .

run:
	@docker run -it --rm htest:latest console

deploy: docker
	@docker save -o /tmp/$(APPNAME).tar $(APPNAME):latest
	@rsync -avzC /tmp/$(APPNAME).tar $(TARGETHOST):.
	@ssh $(TARGETHOST) "docker load -i $(APPNAME).tar"
	@-ssh $(TARGETHOST) "docker kill $(APPNAME)" &> /dev/null || true
	@-ssh $(TARGETHOST) "docker rm $(APPNAME)" &> /dev/null || true

try:
	@ssh -t $(TARGETHOST) "docker run -it --rm --device=/dev/ttyACM0 --privileged --log-driver=journald --log-opt tag=htest --name htest htest:latest console"
