ARG VERSION

FROM silex/emacs:$VERSION

COPY scripts/docker-install.bash scripts/symlink-dotfiles.bash /tmp/
RUN /tmp/docker-install.bash

CMD bash
