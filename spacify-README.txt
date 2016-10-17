Include the local .gitconfig file :
git config --local include.path ../.gitconfig

Force a run of the filter:
rm -rf .git/index && git checkout HEAD -- **


