ARG VERSION
FROM silex/emacs:$VERSION

ARG UID

COPY scripts/docker-install.bash /tmp/
RUN /tmp/docker-install.bash "$UID"

USER $UID
WORKDIR /home/docker/radian

# If we don't do this, then the directory gets created in the
# container filesystem with root ownership :/
RUN mkdir -p "$HOME/.emacs.d/straight/repos"

CMD bash
