# Okay, so here's the deal. Ideally we just want to source ~/.profile
# in ~/.zshenv, which gets sourced in every shell invocation.
# Unfortunately, macOS provides an /etc/profile which sets the default
# PATH, and this file gets sourced *after* ~/.zshenv apparently, which
# means the PATH we just set up in ~/.profile gets messed up. So
# instead we need to use ~/.zprofile, which gets sourced after
# /etc/profile. Unfortunately, ~/.zprofile doesn't get sourced in
# Linux terminal emulators (presumably because they don't start login
# shells, while macOS terminal emulators do for some reason). Thus we
# need to use both, but for efficiency we read and write an
# (unexported) environment variable RADIAN_SKIP_PROFILE to make sure
# that we only source ~/.profile once in any given shell session.

emulate sh -c '. "$HOME/.profile"'
RADIAN_SKIP_PROFILE=1
