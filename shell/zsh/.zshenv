# See .zprofile for some explanation of what is going on here.

if [ -z "$RADIAN_SKIP_PROFILE" ]; then
    emulate sh -c '. "$HOME/.profile"'
else
    RADIAN_SKIP_PROFILE=
fi
