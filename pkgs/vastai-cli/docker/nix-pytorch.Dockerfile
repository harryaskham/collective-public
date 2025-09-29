FROM vastai/pytorch:2.8.0-cuda-12.9.1-py312-24.04
WORKDIR /opt/workspace-internal/
RUN . /venv/main/bin/activate
RUN apt update -y
RUN apt install curl -y
RUN curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install linux \
  --extra-conf "sandbox = false" \
  --init none \
  --no-confirm
ENV PATH="${PATH}:/nix/var/nix/profiles/default/bin"
RUN nix run home-manager/master -- init --switch
RUN echo "source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" >> $HOME/.bashrc
RUN echo "source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" >> $HOME/.zshrc