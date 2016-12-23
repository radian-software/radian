#!/usr/bin/env python
# coding: utf-8 (some Unicode literals are used to print dependency trees)

from __future__ import print_function

import collections
import datetime
import errno
import json
import os
import pipes
import re
import subprocess
import sys
import time

from distutils.version import LooseVersion as Version


################################################################################
#### Environment setup


original_working_directory = os.getcwd()
os.chdir(os.path.split(__file__)[0] or ".")


################################################################################
#### Global variables


verify = True
strict_verify = False
interactive = True
backup_folder = None


################################################################################
#### Exceptions


class SetupError(Exception):
    pass


class CommandUnsuccessfulError(SetupError):
    def __init__(self, args, output, return_code):
        super(CommandUnsuccessfulError, self).__init__(
            "command ({}) exited with return code {}"
            .format(format_shell_command(args), return_code))
        self.cmd_args = args
        self.output = output
        self.return_code = return_code


class NoVersionReportedError(SetupError):
    def __init__(self, args, output, return_code):
        super(NoVersionReportedError, self).__init__(
            "command ({}) did not report a valid version"
            .format(format_shell_command(args)))
        self.cmd_args = args
        self.output = output
        self.return_code = return_code


class AssertionFailedError(SetupError):
    pass


class UserCanceledError(SetupError):
    pass


class UserError(SetupError):
    pass


################################################################################
#### General utility functions


def require(fact, reason):
    if not fact:
        raise AssertionFailedError(reason)


def run_hook(hooks, hook):
    return hooks and hook in hooks and hooks[hook]()


def wait_for_user(message):
    raw_input(">>> {}".format(message))


def get_user_input(message, default=None):
    if default is not None:
        if default:
            return raw_input(">>> {} (default {}): ".format(message, default)) or default
        else:
            return raw_input(">>> {}: ".format(message))
    while True:
        response = raw_input(">>> {}: ".format(message))
        if response:
            return response
        user_message("You must provide an answer.")


def user_confirms(message, default=False):
    if default:
        return not raw_input(">>> {} [Y/n] ".format(message)).lower().startswith("n")
    else:
        return raw_input(">>> {} [y/N] ".format(message)).lower().startswith("y")


def user_message(message):
    print(">>> {}".format(message))


def warn_about_java_popup():
    user_message("Please ignore the inappropriate popup window.")
    wait_for_user("Press RET to continue.")


################################################################################
#### Functions for formatting shell commands


def format_shell_command(args):
    if isinstance(args, basestring):
        return args
    else:
        return " ".join(pipes.quote(arg) for arg in args)


def get_main_command(args):
    if args is None:
        return None
    elif isinstance(args, basestring):
        return args
    else:
        return args[0]


################################################################################
#### Functions for calling subprocesses


def call(args, verify, working_directory=None, display_args=None,
         fork=False, capture=True):
    display_args = display_args or args
    print("$ " + format_shell_command(display_args), end="")
    if verify:
        raw_input()
    else:
        print()
    if fork:
        subprocess.call(args)
    else:
        try:
            if capture:
                process = subprocess.Popen(args, cwd=working_directory,
                                           stdout=subprocess.PIPE,
                                           stderr=subprocess.STDOUT,
                                           bufsize=1)
                output = []
                with process.stdout:
                    for line in iter(process.stdout.readline, b""):
                        line = line.rstrip("\n")
                        print(line)
                        output.append(line)
                return_code = process.wait()
            else:
                output = []
                return_code = subprocess.call(args, cwd=working_directory)
            if return_code != 0:
                print("[Return code: {}]".format(return_code))
            return output, return_code
        except OSError as e:
            if e.errno == errno.ENOENT:
                print("[Command not found: {}]".format(args[0]))
                return None, None
            else:
                raise


def call_require_success(args, verify, display_args=None, capture=True):
    output, return_code = call(args, verify=verify, display_args=display_args,
                               capture=capture)
    if return_code != 0:
        raise CommandUnsuccessfulError(display_args or args, output, return_code)
    return output


def call_with_success(args, verify, display_args=None, capture=True):
    output, return_code = call(args, verify=verify, display_args=display_args,
                               capture=capture)
    if return_code != 0:
        return None
    return output


def call_get_output(args, verify, display_args=None):
    output, return_code = call(args, verify=verify, display_args=display_args)
    return output


def call_get_return_code(args, verify, display_args=None):
    output, return_code = call(args, verify=verify, display_args=display_args,
                               capture=False)
    return return_code


def call_get_success(args, verify, display_args=None):
    output, return_code = call(args, verify=verify, display_args=display_args,
                               capture=False)
    return return_code == 0


def start_new_shell():
    call(["zsh", "-l"], verify=strict_verify, working_directory=original_working_directory,
         fork=True)
    print("Warning: you are returning to the shell you were in before you ran the setup script!")
    exit(1)


################################################################################
#### Functions for checking the versions of things


version_regex = r"(?:[0-9]|HEAD).*?(?:\s|$)"


def extract_version(version_output, prefix=None):
    for line in version_output:
        if prefix and prefix in line:
            line = re.sub(r".*?" + re.escape(prefix), "", line)
        match = re.search(version_regex, line)
        if match:
            return Version(match.group())
    return None


def extract_versions(version_output, prefix=None):
    matches = []
    for line in version_output:
        if prefix in line:
            line = re.sub(r".*?" + prefix, "", line)
        matches.extend(re.findall(version_regex, line))
    return matches


def get_linked_version_of_homebrew_package(package_name, cask=False, tap=None):
    if cask:
        ensure_homebrew_cask_tapped()
        info_output = call_get_output(["brew", "cask", "info", package_name],
                                      verify=strict_verify)
        return extract_version(info_output, prefix=package_name)
    else:
        json_output = call_require_success(["brew", "info", "--json=v1",
                                            package_name],
                                           verify=strict_verify)
        json_data = json.loads(json_output[0])
        if json_data[0][u"keg_only"]:
            return get_latest_installed_version_of_homebrew_package(package_name, cask=cask,
                                                                    tap=tap)
        else:
            version = json_data[0][u"linked_keg"]
            return version and Version(version)


def get_latest_installed_version_of_homebrew_package(package_name, cask=False, tap=None,
                                                     remember=False, memory={}):
    key = package_name, cask, tap
    if remember and key in memory:
        return memory[key]
    brew = ["brew"]
    if cask:
        ensure_homebrew_cask_tapped()
        brew.append("cask")
    versions_output = call_get_output(brew + ["list", "--versions",
                                              package_name],
                                      verify=strict_verify)
    versions = extract_versions(versions_output, prefix=package_name)
    result = versions and max(versions)
    memory[key] = result
    return result


def get_insecure_compinit_directories(remember=False, memory={}):
    if remember and "dirs" in memory:
        return memory["dirs"]
    output, return_code = call(["zsh", "-c", "autoload -Uz compaudit; compaudit"], verify=strict_verify)
    if return_code == 0:
        memory["dirs"] = []
        return []
    if output and output[0] == "There are insecure directories:":
        memory["dirs"] = output[1:]
        return output[1:]
    raise AssertionFailedError("compaudit did not work as expected")


def get_zsh(remember=False, memory={}):
    if remember and "zsh" in memory:
        return memory["zsh"]
    zsh = call_require_success(["which", "zsh"], verify=strict_verify)[0]
    memory["zsh"] = zsh
    return zsh


################################################################################
#### Functions for checking if things are installed


def is_command_available(command, working_directory=None, allow_nonzero_return=False,
                         minimum_version=None, prefix=None):
    output, return_code = call(command, verify=strict_verify,
                               working_directory=working_directory)
    if return_code is None or not allow_nonzero_return and return_code != 0:
        return False
    if not minimum_version:
        return True
    version = extract_version(output, prefix=prefix)
    return version and version >= minimum_version


def are_xcode_command_line_tools_installed():
    return call_get_success(["xcode-select", "-p"],
                            verify=strict_verify)


def is_homebrew_package_installed(package_name, cask=False, tap=None, flags=[],
                                  minimum_version=None):
    latest_installed_version = get_linked_version_of_homebrew_package(
        package_name, cask=cask, tap=tap)
    if latest_installed_version:
        return ((not minimum_version or latest_installed_version >= minimum_version) and
                ("--HEAD" not in flags or str(latest_installed_version).startswith("HEAD")))
    else:
        return False


def are_all_insecure_compinit_directories_fixed():
    return not get_insecure_compinit_directories()


def is_zsh_a_login_shell():
    with open("/etc/shells", "r") as f:
        for line in f:
            if line.rstrip("\n") == get_zsh(remember=True):
                return True
    return False


def is_zsh_the_login_shell():
    info_output = call_require_success(["finger", os.environ["USER"]],
                                       verify=strict_verify)
    for line in info_output:
        match = re.search(r"Shell: (.+)", line)
        if match:
            return match.group(1) == get_zsh(remember=True)
    raise AssertionFailedError("finger did not report a shell")


################################################################################
#### Functions for installing things


def install_xcode_command_line_tools():
    call_require_success(["xcode-select", "--install"],
                         verify=verify, capture=False)
    time.sleep(5)
    wait_for_user("Press RET when you have finished installing the Command Line Tools.")


def install_homebrew():
    url = "https://raw.githubusercontent.com/Homebrew/install/master/install"
    install_script = "\n".join(call_require_success(["curl", "-fsSL", url],
                                                    verify=verify))
    call_require_success(["/usr/bin/ruby", "-e", install_script],
                         display_args=["/usr/bin/ruby", "-e", "<script>"],
                         verify=verify, capture=False)


def install_homebrew_package(package_name, cask=False, tap=None, flags=[],
                             minimum_version=None, also=None):
    full_package_name = (tap + "/" + package_name
                         if tap else package_name)
    brew = ["brew"]
    ensure_homebrew_updated()
    if cask:
        ensure_homebrew_cask_tapped()
        brew.append("cask")
    latest_installed_version = get_latest_installed_version_of_homebrew_package(
        package_name, cask=cask, tap=tap, remember=True)
    if latest_installed_version:
        if ((not minimum_version or latest_installed_version >= minimum_version) and
            ("--HEAD" not in flags or str(latest_installed_version).startswith("HEAD"))):
            call_require_success(brew + ["switch", package_name,
                                         str(latest_installed_version)],
                                 verify=verify, capture=False)
            return
        else:
            call_require_success(brew + ["uninstall", "--force", package_name],
                                 verify=verify, capture=False)
    call_require_success(brew + ["install"] + flags + [full_package_name],
                         verify=verify, capture=False)


def install_rubygem(gem_name):
    call_require_success(["sudo", "gem", "install", gem_name],
                         verify=verify, capture=False)


def fix_insecure_compinit_directories():
    dirs = get_insecure_compinit_directories(remember=True)
    if dirs:
        call_require_success(["chmod", "g-w"] + dirs, verify=verify, capture=False)


def make_zsh_a_login_shell():
    zsh = get_zsh(remember=True)
    call_require_success(["sudo", "scripts/addshell", zsh],
                         verify=verify)


def make_zsh_the_login_shell():
    call_require_success(["chsh", "-s", get_zsh(remember=True)],
                         verify=verify, capture=False)


################################################################################
#### Functions for ensuring things are installed


def ensure_installed(install, is_installed=None, is_properly_installed=None,
                     not_installed_error=None, not_properly_installed_error=None,
                     hooks=None):
    def install_with_hooks():
        run_hook(hooks, "before_install")
        install()
        run_hook(hooks, "after_install")
    def is_installed_with_hooks():
        run_hook(hooks, "before_is_installed")
        result = None
        if is_installed:
            result = is_installed()
        if run_hook(hooks, "retry_is_installed") and is_installed:
            result = is_installed()
        run_hook(hooks, "after_is_installed")
        if result is True:
            run_hook(hooks, "after_is_installed_true")
        elif result is False:
            run_hook(hooks, "after_is_installed_false")
        return result
    def is_properly_installed_with_hooks():
        run_hook(hooks, "before_is_properly_installed")
        result = None
        if is_properly_installed:
            result = is_properly_installed()
        if run_hook(hooks, "retry_is_properly_installed") and is_properly_installed:
            result = is_properly_installed()
        run_hook(hooks, "after_is_properly_installed")
        if result is True:
            run_hook(hooks, "after_is_properly_installed_true")
        elif result is False:
            run_hook(hooks, "after_is_properly_installed_false")
        return result
    if (not is_installed and not is_properly_installed or
        is_installed and not is_installed_with_hooks() or
        is_properly_installed and not is_properly_installed_with_hooks()):
        install_with_hooks()
        if is_installed and not is_installed_with_hooks():
            raise AssertionFailedError(
                not_installed_error or "installation failed")
        if is_properly_installed and not is_properly_installed_with_hooks():
            raise AssertionFailedError(
                not_properly_installed_error or "installation failed")
        return True
    return False


def get_not_installed_error(what, name=None, version=None):
    if name:
        name_with_space = " " + name
    else:
        name_with_space = ""
    if version:
        return ("version {} of {}{} should have been installed by now"
                .format(version, what, name_with_space))
    else:
        return ("{}{} should have been installed by now"
                .format(what, name_with_space))


def ensure_xcode_command_line_tools_installed():
    return ensure_installed(
        install=install_xcode_command_line_tools,
        is_installed=are_xcode_command_line_tools_installed,
        not_installed_error=get_not_installed_error(what="Xcode Command Line Tools"))


def ensure_homebrew_installed():
    return ensure_installed(
        install=install_homebrew,
        is_installed=lambda: is_command_available(["brew", "--version"]),
        not_installed_error=get_not_installed_error(what="Homebrew"))


def ensure_homebrew_cask_tapped(memory={"done": False}):
    if not memory["done"]:
        call_require_success(["brew", "cask"], verify=strict_verify, capture=False)
        memory["done"] = True


def ensure_homebrew_updated(memory={"done": False}):
    if not memory["done"]:
        call_require_success(["brew", "update"], verify=strict_verify, capture=False)
        memory["done"] = True


def ensure_homebrew_package_installed(package_name, cask=False, tap=None, flags=[],
                                      command=None, working_directory=None,
                                      allow_nonzero_return=False,
                                      minimum_version=None, prefix=None,
                                      require_homebrew=False, hooks=None):
    return ensure_installed(
        install=lambda: install_homebrew_package(package_name, cask=cask, tap=tap, flags=flags,
                                                 minimum_version=minimum_version),
        is_installed=(command and
                      (lambda: is_command_available(command, working_directory=working_directory,
                                                    allow_nonzero_return=allow_nonzero_return,
                                                    minimum_version=minimum_version, prefix=prefix))),
        is_properly_installed=((require_homebrew or not command) and
                               (lambda: is_homebrew_package_installed(package_name, cask=cask, tap=tap,
                                                                      flags=flags, minimum_version=minimum_version))),
        not_installed_error=get_not_installed_error(what="command",
                                                    name=get_main_command(command),
                                                    version=minimum_version),
        not_properly_installed_error=get_not_installed_error(what="package",
                                                             name=package_name,
                                                             version=minimum_version),
        hooks=hooks)


def ensure_rubygem_installed(gem_name, command=None, minimum_version=None, prefix=None,
                             hooks=None):
    return ensure_installed(
        install=lambda: install_rubygem(gem_name),
        is_installed=(command and
                      (lambda: is_command_available(command, minimum_version=minimum_version,
                                                    prefix=prefix))),
        not_installed_error=get_not_installed_error(what="gem",
                                                    name=gem_name,
                                                    version=minimum_version),
        hooks=hooks)


def ensure_no_insecure_compinit_directories():
    return ensure_installed(
        install=fix_insecure_compinit_directories,
        is_installed=are_all_insecure_compinit_directories_fixed,
        not_installed_error="there shouldn't be any insecure compinit directories by now")


def ensure_zsh_is_a_login_shell():
    return ensure_installed(
        install=make_zsh_a_login_shell,
        is_installed=is_zsh_a_login_shell,
        not_installed_error="zsh should be in /etc/shells by now")


def ensure_zsh_is_the_login_shell():
    return ensure_installed(
        install=make_zsh_the_login_shell,
        is_installed=is_zsh_the_login_shell,
        not_installed_error="zsh should the login shell by now")


################################################################################
#### Utility functions for dealing with paths


def home(path=None):
    if path:
        return os.path.join(os.path.expanduser("~"), path)
    else:
        return os.path.expanduser("~")


def local(path=None):
    if path:
        return os.path.join("../radian-local", path)
    else:
        return "../radian-local"


################################################################################
#### Functions for checking the state of the filesystem


def does_path_exist(path):
    # The first condition returns False for broken symlinks, hence the
    # second condition.
    return os.path.exists(path) or os.path.islink(path)


def is_path_file(path):
    return os.path.isfile(path)


def is_path_nonfile(path):
    return does_path_exist(path) and not is_path_file(path)


def is_path_directory(path):
    return os.path.isdir(path)


def is_path_nondirectory(path):
    return does_path_exist(path) and not is_path_directory(path)


def is_path_empty_directory(path):
    return is_path_directory(path) and not os.listdir(path)


def is_symlinked(symlink_path, target_path):
    return os.path.realpath(symlink_path) == os.path.realpath(target_path)


def is_directory_version_controlled(directory):
    return is_path_directory(os.path.join(directory, ".git"))


################################################################################
#### Functions for modifying the state of the filesystem


def create_directory(directory, strict_verify_only=False):
    call_require_success(["mkdir", "-p", directory],
                         verify=strict_verify if strict_verify_only else verify,
                         capture=False)


def create_symlink(symlink_path, target_path):
    call_require_success(["ln", "-s", os.path.realpath(target_path), symlink_path],
                         verify=verify, capture=False)


def back_up_path(path, strict_verify_only=False):
    filename = os.path.split(path)[1]
    ensure_backup_folder_exists(strict_verify_only=strict_verify_only)
    call_require_success(["mv", path, os.path.join(backup_folder, filename)],
                         verify=strict_verify if strict_verify_only else verify,
                         capture=False)


def restore_path(path, strict_verify_only=False):
    filename = os.path.split(path)[1]
    assert backup_folder
    call_require_success(["mv", os.path.join(backup_folder, filename), path],
                         verify=strict_verify if strict_verify_only else verify,
                         capture=False)


################################################################################
#### Functions for ensuring the state of the filesystem


def ensure_symlinked(symlink_path, target_path):
    if not is_symlinked(symlink_path, target_path):
        if does_path_exist(symlink_path):
            back_up_path(symlink_path)
        create_symlink(symlink_path, target_path)
        return True
    return False


def ensure_path_is_file(path, get_text):
    if is_path_nonfile(path):
        back_up_path(path)
    if not is_path_file(path):
        while True:
            text = get_text()
            print("===== BEGIN TEXT =====")
            print(text, end="")
            print("===== END TEXT =====")
            if user_confirms(("Write this to file {}?"
                              .format(path)),
                             default=True):
                with open(path, "w") as f:
                    f.write(text)
                return True
            elif not user_confirms("Try again?", default=True):
                raise UserCanceledError
    return False


def ensure_path_is_directory(path, request_unversioned=False, strict_verify_only=False):
    if is_path_nondirectory(path):
        back_up_path(path, strict_verify_only, strict_verify_only)
    if request_unversioned and is_directory_version_controlled(path):
        if user_confirms(("Directory {} is version-controlled. Move it out of the way?"
                          .format(path)),
                         default=True):
            back_up_path(path, strict_verify_only=strict_verify_only)
    if not is_path_directory(path):
        create_directory(path, strict_verify_only=strict_verify_only)
        return True
    return False


def ensure_backup_folder_exists(strict_verify_only=False):
    global backup_folder
    if not backup_folder:
        uuid = str(datetime.datetime.now().utcnow())
        backup_folder = os.path.join("backups", uuid)
    return ensure_path_is_directory(backup_folder, strict_verify_only=strict_verify_only)


def ensure_path_backed_up(path, strict_verify_only=False):
    if does_path_exist(path):
        back_up_path(path, strict_verify_only=strict_verify_only)
        return True
    return False


def ensure_path_restored(path, strict_verify_only=False):
    filename = os.path.split(path)[1]
    backed_up_path = os.path.join(backup_folder, filename)
    if does_path_exist(backed_up_path):
        restore_path(path, strict_verify_only=strict_verify_only)
        return True
    return False


def request_running_in_repo():
    if "true" not in call_get_output(["git", "rev-parse", "--is-inside-work-tree"],
                                     verify=strict_verify):
        if user_confirms("Radian is not a Git repo. Would you like to make it one?",
                         default=True):
            call_require_success(["git", "init"], verify=verify)
            while True:
                repo = get_user_input("Enter author/repo", "raxod502/radian")
                url = "https://github.com/" + repo + ".git"
                call_require_success(["git", "remote", "add", "origin", url], verify=verify)
                if call_get_success(["git", "fetch", "origin"], verify=verify):
                    break
                call_require_success(["git", "remote", "remove", "origin"], verify=verify)
            while True:
                revision = get_user_input("Enter revision or branch", "master")
                if call_get_success(["git", "checkout", "--force", revision],
                                    verify=verify):
                    break
            return True
    return False


def ensure_backup_folder_pruned():
    if is_path_directory("backups"):
        for entry in os.listdir("backups"):
            path = os.path.join("backups", entry)
            if is_path_empty_directory(path):
                call_require_success(["rmdir", path], verify=strict_verify,
                                     capture=False)


################################################################################
#### Functions for generating file text


def text(filename_or_iterable):
    def get_text():
        if isinstance(filename_or_iterable, basestring):
            with open(filename_or_iterable, "r") as f:
                return f.read()
        else:
            return "".join(line + "\n" for line in filename_or_iterable())
    return get_text


def gitconfig_local():
    name = (call_with_success(["git", "config", "--global",
                               "--includes", "user.name"],
                              verify=strict_verify) or
            get_user_input("Please enter your full name for Git"))
    email = (call_with_success(["git", "config", "--global",
                                "--includes", "user.email"],
                               verify=strict_verify) or
             get_user_input("Please enter your email address for Git"))
    editor = call_with_success(["git", "config", "--global",
                                "--includes", "core.editor"],
                               verify=strict_verify)
    yield "[user]"
    yield "\tname = {}".format(name)
    yield "\temail = {}".format(email)
    if editor:
        yield "[core]"
        yield "\teditor = {}".format(editor)


def zshrc_local():
    with open("templates/.zshrc.local", "r") as f:
        for line in f:
            yield line.rstrip("\n")
    if "EDITOR" in os.environ:
        yield ""
        yield "export EDITOR={}".format(pipes.quote(os.environ["EDITOR"]))


################################################################################
#### Functions for performing setup


def setup_leiningen():
    try:
        ensure_path_backed_up(home(".lein/profiles.clj"),
                              strict_verify_only=True)
        ensure_homebrew_package_installed("leiningen", command=["lein", "--version"],
                                          working_directory=home(),
                                          minimum_version=Version("2.6.1"))
    except:
        user_message("An error occurred. Let's make sure to restore profiles.clj.")
        raise
    finally:
        try:
            ensure_path_restored(home(".lein/profiles.clj"),
                                 strict_verify_only=True)
        except Exception:
            pass


################################################################################
#### Feature definitions


feature_list = [

    ### Housekeeping ###

    {"name": "prune-backups-before",
     "pretty_name": "Prune backups before starting",
     "description": "Remove empty backup folders.",
     "group": "housekeeping-before",
     "hidden": True,
     "default": True,
     "action": lambda: ensure_backup_folder_pruned()},

    {"name": "radian-local",
     "pretty_name": "radian-local folder",
     "description": "Create folder for local dotfiles.",
     "group": "housekeeping-before",
     "hidden": True,
     "default": True,
     "action": lambda: ensure_path_is_directory(local())},

    ### Basic stuff ###

    {"name": "xcode-cl-tools",
     "pretty_name": "Xcode Command Line Tools",
     "description": "Required for basic command-line utilities like Git to work on macOS.",
     "group": "basic",
     "action": lambda: ensure_xcode_command_line_tools_installed()},

    {"name": "homebrew",
     "pretty_name": "Homebrew",
     "description": "The missing package manager for macOS. Can install almost everything.",
     "group": "basic",
     "action": lambda: ensure_homebrew_installed()},

    ### Git ###

    {"name": "git-config",
     "pretty_name": "Git configuration",
     "description": "Radian's Git configuration (.gitconfig, .gitexclude, and .gitconfig.local).",
     "group": "git",
     "required": ["radian-local", "xcode-cl-tools"],
     "recommended": ["hub"],
     "action": lambda: (
         ensure_path_is_file(local(".gitconfig.local"), text(gitconfig_local)),
         ensure_symlinked(home(".gitconfig"), ".gitconfig"),
         ensure_symlinked(home(".gitexclude"), ".gitexclude"),
         ensure_symlinked(home(".gitconfig.local"), local(".gitconfig.local")))},

    ### Zsh ###

    {"name": "zsh",
     "pretty_name": "Zsh",
     "description": "Awesome shell, better than Bash. Has an extensive plugin ecosystem.",
     "group": "zsh",
     "required": ["radian-local", "homebrew"],
     "recommended": ["zsh-login-shell", "zsh-config", "new-shell"],
     "action": lambda: (
         ensure_homebrew_package_installed("zsh", tap="raxod502/radian",
                                           command=["zsh", "--version"],
                                           minimum_version=Version("5.2")),
         ensure_no_insecure_compinit_directories())},

    {"name": "zsh-login-shell",
     "pretty_name": "Set Zsh as login shell",
     "description": "Set Zsh as your login shell.",
     "group": "zsh",
     "required": ["zsh"],
     "action": lambda: (
         ensure_zsh_is_a_login_shell(),
         ensure_zsh_is_the_login_shell())},

    {"name": "zplug",
     "pretty_name": "zplug",
     "description": "Fast plugin manager for Zsh. Installs and loads specified plugins automatically.",
     "group": "zsh",
     "required": ["homebrew", "zsh"],
     "action": lambda: (
         ensure_homebrew_package_installed("zplug", flags=["--HEAD"], minimum_version=Version("2.3.4"),
                                           require_homebrew=True))},

    {"name": "zsh-config",
     "pretty_name": "Zsh configuration",
     "description": "Radian's Zsh configuration (.zshrc, .zshrc.before.local, and .zshrc.local)",
     "group": "zsh",
     "required": ["radian-local", "zsh", "zplug"],
     "recommended": ["exa", "fasd", "hub"],
     "action": lambda: (
         ensure_path_is_file(local(".zshrc.before.local"), text("templates/.zshrc.before.local")),
         ensure_path_is_file(local(".zshrc.local"), text(zshrc_local)),
         ensure_symlinked(home(".zshrc"), ".zshrc"),
         ensure_symlinked(home(".zshrc.before.local"), local(".zshrc.before.local")),
         ensure_symlinked(home(".zshrc.local"), local(".zshrc.local")))},

    ### Tmux ###

    {"name": "tmux",
     "pretty_name": "tmux",
     "description": "The terminal multiplexer. Organize and freely attach/detach from multiple terminals.",
     "group": "tmux",
     "required": ["radian-local", "homebrew"],
     "recommended": ["reattach-to-user-namespace", "tmux-config", "tmuxinator"],
     "action": lambda: (
         ensure_homebrew_package_installed("tmux", command=["tmux", "-V"],
                                           minimum_version=Version("2.2")))},

    {"name": "reattach-to-user-namespace",
     "pretty_name": "Tmux macOS Pasteboard Re-Enabler",
     "description": "Small wrapper program that works around a bug preventing pasteboard access inside tmux.",
     "group": "tmux",
     "required": ["homebrew"],
     "action": lambda: (
         ensure_homebrew_package_installed("reattach-to-user-namespace",
                                           command=["reattach-to-user-namespace", "--version"]))},

    {"name": "tmux-config",
     "pretty_name": "Tmux configuration",
     "description": "Radian's Tmux configuration (.tmux.conf and .tmux.local.conf).",
     "group": "tmux",
     "required": ["radian-local", "tmux", "reattach-to-user-namespace"],
     "action": lambda: (
         ensure_path_is_file(local(".tmux.local.conf"), text("templates/.tmux.local.conf")),
         ensure_symlinked(home(".tmux.conf"), ".tmux.conf"),
         ensure_symlinked(home(".tmux.local.conf"), local(".tmux.local.conf")))},

    ### Emacs ###

    {"name": "emacs",
     "pretty_name": "GNU Emacs",
     "description": "The extensible, customizable, self-documenting real-time display editor.",
     "group": "emacs",
     "required": ["homebrew"],
     "action": lambda: (
         ensure_homebrew_package_installed("emacs", cask=True, command=["emacs", "--version"],
                                           minimum_version=Version("25.1"),
                                           hooks={"retry_is_installed": lambda: ensure_symlinked("/usr/local/bin/emacs",
                                                                                                 "scripts/emacs")}))},

    {"name": "emacs-config",
     "pretty_name": "Emacs configuration",
     "description": "Radian's Emacs configuration: (init.el, init.before.local.el, and init.local.el).",
     "group": "emacs",
     "required": ["radian-local", "emacs"],
     "recommended": ["ag", "fasd", "leiningen", "cmake", "libclang", "racket"],
     "action": lambda: (
         ensure_path_backed_up(home(".emacs")),
         ensure_path_backed_up(home(".emacs.el")),
         ensure_path_is_file(local("init.before.local.el"), text("templates/init.before.local.el")),
         ensure_path_is_file(local("init.local.el"), text("templates/init.local.el")),
         ensure_path_is_directory(home(".emacs.d"), request_unversioned=True),
         ensure_symlinked(home(".emacs.d/init.el"), "init.el"),
         ensure_symlinked(home(".emacs.d/init.before.local.el"), local("init.before.local.el")),
         ensure_symlinked(home(".emacs.d/init.local.el"), local("init.local.el")))},

    ### Vim ###

    {"name": "vim",
     "pretty_name": "Vim",
     "description": "The ubiquitous text editor. Power tool for editing any kind of text.",
     "group": "vim",
     "required": ["homebrew"],
     "action": lambda: (
         ensure_homebrew_package_installed("neovim", tap="neovim/neovim", command=["nvim", "--version"],
                                           minimum_version=Version("0.1.6")))},

    {"name": "vim-config",
     "pretty_name": "Vim configuration",
     "description": "Radian's Vim configuration: (init.vim).",
     "group": "vim",
     "required": ["radian-local", "vim"],
     "action": lambda: (
         ensure_path_is_directory(home(".config"), request_unversioned=True),
         ensure_path_is_directory(home(".config/nvim"), request_unversioned=True),
         ensure_symlinked(home(".config/nvim/init.vim"), "init.vim"))},

    ### Java ###

    {"name": "java",
     "pretty_name": "Java",
     "description": "The programming language. Clojure runs on the Java Virtual Machine.",
     "group": "java",
     "required": ["homebrew"],
     "action": lambda: (
         ensure_homebrew_package_installed("java", cask=True, command=["javac", "-version"],
                                           minimum_version=Version("1.6"),
                                           hooks={"after_is_installed_false": warn_about_java_popup}))},

    ### Clojure ###

    {"name": "leiningen",
     "pretty_name": "Leiningen",
     "description": "De-facto standard build manager for Clojure. Includes the Clojure language.",
     "group": "clojure",
     "required": ["homebrew", "java"],
     "recommended": ["leiningen-config"],
     "action": lambda: setup_leiningen()},

    {"name": "leiningen-config",
     "pretty_name": "Leiningen configuration",
     "description": "Radian's Leiningen configuration (profiles.clj).",
     "group": "clojure",
     "required": ["radian-local", "leiningen"],
     "action": lambda: (ensure_path_is_directory(home(".lein"), request_unversioned=True),
                        ensure_symlinked(home(".lein/profiles.clj"), "profiles.clj"))},

    ### C++ ###

    {"name": "cmake",
     "pretty_name": "CMake",
     "description": "Cross-platform C++ build manager. Required for C++ development in Emacs.",
     "group": "c++",
     "required": ["homebrew"],
     "action": lambda: (
         ensure_homebrew_package_installed("cmake", command=["cmake", "--version"],
                                           minimum_version=Version("3.7.0")))},

    {"name": "libclang",
     "pretty_name": "libclang",
     "description": "C library for parsing C++ source code. Required for C++ development in Emacs.",
     "group": "c++",
     "required": ["homebrew"],
     "action": lambda: (
         ensure_homebrew_package_installed("llvm", minimum_version=Version("3.9.0"),
                                           require_homebrew=True))},

    ### Racket ###

    {"name": "racket",
     "pretty_name": "Racket",
     "description": "The programming language.",
     "group": "racket",
     "required": ["homebrew"],
     "action": lambda: (
         ensure_homebrew_package_installed("racket", cask=True, command=["racket", "--version"],
                                           minimum_version=Version("6.6")))},

    ### Utilities ###

    {"name": "ag",
     "pretty_name": "Ag",
     "description": "A code searching tool similar to ack, but faster.",
     "group": "utilities",
     "required": ["homebrew"],
     "action": lambda: (
         ensure_homebrew_package_installed("ag", command=["ag", "--version"]))},

    {"name": "coreutils",
     "pretty_name": "GNU core utilities",
     "description": "Basic file, shell, and text manipulation utilities of the GNU operating system.",
     "group": "utilities",
     "required": ["homebrew"],
     "action": lambda: (
         ensure_homebrew_package_installed("coreutils", require_homebrew=True))},

    {"name": "exa",
     "pretty_name": "exa",
     "description": "A modern replacement for ls.",
     "group": "utilities",
     "required": ["homebrew"],
     "action": lambda: (
         ensure_homebrew_package_installed("exa", cask=True, command=["exa", "--version"],
                                           allow_nonzero_return=True))},

    {"name": "fasd",
     "pretty_name": "Fasd",
     "description": "Smart command-line utility to jump to frequently used files and directories.",
     "group": "utilities",
     "required": ["homebrew"],
     "action": lambda: (
         ensure_homebrew_package_installed("fasd", tap="raxod502/radian", command=["fasd", "--version"],
                                           minimum_version=Version("1.0.2")))},

    {"name": "hub",
     "pretty_name": "Hub",
     "description": "A command-line wrapper for git that adds GitHub integration.",
     "group": "utilities",
     "required": ["homebrew"],
     "action": lambda: (
     ensure_homebrew_package_installed("hub", command=["hub", "--version"],
                                       prefix="git version .+"))},

    {"name": "tmuxinator",
     "pretty_name": "tmuxinator",
     "description": "Create and manage tmux sessions by creating template files.",
     "group": "utilities",
     "required": ["homebrew", "tmux"],
     "action": lambda: (
         ensure_rubygem_installed("tmuxinator", command=["tmuxinator", "version"],
                                  minimum_version=Version("0.8.1")))},

    {"name": "wget",
     "pretty_name": "Wget",
     "description": "Software package for downloading files. More convenient than curl.",
     "group": "utilities",
     "required": ["homebrew"],
     "action": lambda: (
         ensure_homebrew_package_installed("wget", command=["wget", "--version"]))},

    ### Housekeeping ###

    {"name": "request-repo",
     "pretty_name": "Request repository",
     "description": "Request that the Radian folder be version-controlled by Git.",
     "hidden": True,
     "default": True,
     "group": "housekeeping-after",
     "required": ["xcode-cl-tools"],
     "action": lambda: request_running_in_repo()},

    {"name": "prune-backups-after",
     "pretty_name": "Prune backups after finishing",
     "description": "Remove empty backup folders.",
     "hidden": True,
     "default": True,
     "group": "housekeeping-after",
     "action": lambda: ensure_backup_folder_pruned()},

    {"name": "new-shell",
     "pretty_name": "Start new shell",
     "description": "Start new shell session after setup is finished.",
     "hidden": True,
     "group": "housekeeping-after",
     "required": ["zsh"],
     "action": lambda: start_new_shell()},

]


groups = {"housekeeping-before": "Housekeeping",
          "basic": "Basic",
          "git": "Git",
          "zsh": "Zsh",
          "tmux": "Tmux",
          "emacs": "Emacs",
          "vim": "Vim",
          "clojure": "Clojure",
          "java": "Java",
          "c++": "C++",
          "racket": "Racket",
          "utilities": "Utilities",
          "housekeeping-after": "Housekeeping"}


################################################################################
#### Feature processing


features = {}


for feature in feature_list:
    assert "name" in feature
    assert "pretty_name" in feature
    assert "description" in feature
    if "hidden" not in feature:
        feature["hidden"] = False
    feature["originally_hidden"] = feature["hidden"]
    if "default" not in feature:
        feature["default"] = False
    assert "group" in feature
    if "required" not in feature:
        feature["required"] = []
    if "recommended" not in feature:
        feature["recommended"] = []
    feature["required_by"] = []
    assert "action" in feature
    feature["enabled"] = feature["default"]
    features[feature["name"]] = feature


for feature in feature_list:
    for dependency_name in feature["required"]:
        features[dependency_name]["required_by"].append(feature["name"])


def get_dependencies(feature, dep_type, filter_fn=lambda x: True,
                     max_depth=None, depth=0, more=[]):
    line = ""
    for m in more[:-1]:
        line += "│  " if m else "   "
    if depth >= 1:
        line += "├── " if more[-1] else "└── "
    line += feature["name"] + ": " + feature["description"]
    if filter_fn(feature):
        deps = [feature]
        lines = [line]
    else:
        deps = []
        lines = []
    if depth != max_depth:
        for index, dep_name in enumerate(feature[dep_type]):
            new_deps, new_lines = get_dependencies(
                features[dep_name], dep_type, filter_fn=filter_fn,
                max_depth=max_depth, depth=depth+1,
                more=more+[index < len(feature[dep_type]) - 1])
            deps += new_deps
            lines += new_lines
    return deps, lines


def toggle_feature(feature, enable, interactive=True):
    if feature["enabled"] != enable:
        required = feature["required"]
        recommended = feature["recommended"]
        if enable:
            deps, lines = get_dependencies(feature, "required",
                                           filter_fn=lambda x: not x["enabled"])
            if sum(not dep["hidden"] for dep in deps[1:]) >= 1 and interactive:
                user_message("Note: {} requires the following features, "
                             "which will also be enabled:"
                             .format(feature["name"]))
                print(lines[0])
                for dep, line in zip(deps, lines)[1:]:
                    if not dep["hidden"]:
                        print(line)
                if not user_confirms("Proceed?", default=True):
                    return
            for dependency in deps:
                dependency["enabled"] = True
            deps, lines = get_dependencies(feature, "recommended",
                                           filter_fn=lambda x: not x["enabled"])
            confirmed = True
            if sum(not dep["hidden"] for dep in deps[1:]) >= 1 and interactive:
                user_message("Note: if you enable {}, the following features "
                             "are also recommended to be enabled:"
                             .format(feature["name"]))
                print(lines[0])
                for dep, line in zip(deps, lines)[1:]:
                    if not dep["hidden"]:
                        print(line)
                confirmed = user_confirms("Enable them?", default=True)
                if not confirmed:
                    user_message("You can enable the features individually if you "
                                 "would like.")
            for dependency in deps:
                if confirmed or dependency["hidden"]:
                    toggle_feature(dependency, enable=True, interactive=interactive)
        else:
            deps, lines = get_dependencies(feature, "required_by",
                                           filter_fn=lambda x: x["enabled"])
            if sum(not dep["hidden"] for dep in deps[1:]) >= 1 and interactive:
                user_message("Note: {} is a requirement for the following "
                             "features, which will also be disabled:"
                             .format(feature["name"]))
                print(lines[0])
                for dep, line in zip(deps, lines)[1:]:
                    if not dep["hidden"]:
                        print(line)
                if not user_confirms("Proceed?", default=True):
                    return
            for dependency in deps:
                dependency["enabled"] = False


def handle_cl_flag(flag):
    global verify, strict_verify, interactive
    if flag == "help":
        print("usage: ./setup.py <flags>")
        print("  +<feature> = enable the feature and its dependencies")
        print("  -<feature> = disable the feature and its dependencies")
        print("  --all = enable all features")
        print("  --none = disable all features")
        print("  --reset = enable only those feature enabled by default")
        print("  --show-hidden = show hidden features")
        print("  --hide-hidden = hide hidden features (default)")
        print("  --verify = confirm dangerous commands before running them (default)")
        print("  --strict-verify = confirm all commands before running them")
        print("  --no-verify = don't confirm commands before running them")
        print("  --interactive = select features interactively (default)")
        print("  --no-interactive = don't select features interactively")
        print("  go = shorthand for --all --no-interactive")
        sys.exit(0)
    elif flag == "all":
        for feature in feature_list:
            toggle_feature(feature, enable=True, interactive=False)
    elif flag == "reset":
        for feature in feature_list:
            feature["enabled"] = feature["default"]
    elif flag == "none":
        for feature in feature_list:
            toggle_feature(feature, enable=False, interactive=False)
    elif flag == "show-hidden":
        for feature in feature_list:
            feature["hidden"] = False
    elif flag == "hide-hidden":
        for feature in feature_list:
            feature["hidden"] = feature["originally_hidden"]
    elif flag == "verify":
        verify = True
        strict_verify = False
    elif flag == "strict-verify":
        verify = True
        strict_verify = True
    elif flag == "no-verify":
        verify = True
    elif flag == "interactive":
        interactive = True
    elif flag == "no-interactive":
        interactive = False
    else:
        return False
    return True


################################################################################
#### Setup script


def setup():
    args = sys.argv[1:]
    for arg in args:
        if arg == "go":
            if len(args) > 1:
                raise UserError("can't use 'go' with other arguments")
            handle_cl_flag("all")
            handle_cl_flag("no-interactive")
        elif arg.startswith("--"):
            if not handle_cl_flag(arg[2:]):
                raise UserError("Invalid flag: {}".format(arg))
        elif arg.startswith("+") or arg.startswith("-"):
            enable = arg.startswith("+")
            feature_name = arg[1:]
            if not feature_name:
                raise UserError("'{}' must be followed by a feature name"
                                .format(arg[0]))
            if feature_name not in features:
                raise UserError("'{}' is not a valid feature name")
            toggle_feature(features[feature_name], enable=enable, interactive=False)
        else:
            raise UserError("'{}' is not a valid argument".format(arg))
    if interactive:
        show_hint = True
        show_features = True
        history = []
        while True:
            if show_features:
                user_message("Features (shown with (*) if enabled):")
                last_group = None
                for feature in feature_list:
                    if not feature["hidden"]:
                        if feature["group"] != last_group:
                            user_message("  {}:".format(feature["group"]))
                            last_group = feature["group"]
                        user_message("   {} {}: {}".format("(*)" if feature["enabled"]
                                                           else "   ",
                                                           feature["name"],
                                                           feature["description"]))
                user_message("Verification status: {}"
                             .format("strict" if strict_verify else
                                     ("default" if verify else "none")))
                show_features = False
            hint = ""
            if show_hint:
                hint = " (or 'help' for help)"
            cmd = get_user_input("Enter a command" + hint, default="").split()
            if not cmd:
                continue
            elif cmd[0] in ("enable", "disable"):
                enable = cmd[0] == "enable"
                if len(cmd) > 2:
                    user_message("Too many arguments.")
                    continue
                if cmd[1] not in features:
                    user_message("That is not the name of a valid feature.")
                    continue
                history.append(("+" if enable else "-") + cmd[1])
                toggle_feature(features[cmd[1]], enable=enable, interactive=True)
            elif cmd[0] == "go":
                if sum(feature["enabled"] for feature in feature_list) == 0:
                    user_message("Nothing to do!")
                    continue
                else:
                    break
            elif cmd[0] == "quit":
                sys.exit(1)
            else:
                if len(cmd) > 1:
                    user_message("Too many arguments.")
                    continue
                elif cmd[0] == "help":
                    user_message("Commands:")
                    user_message("  enable <feature>")
                    user_message("  disable <feature>")
                    user_message("  go = proceed with setup")
                    user_message("  quit = abort")
                    user_message("  all = enable all features")
                    user_message("  none = disable all features")
                    user_message("  reset = enable only features enabled by default")
                    user_message("  show-hidden = show hidden features")
                    user_message("  hide-hidden = hide hidden features (default)")
                    user_message("  verify = confirm dangerous commands before they are run")
                    user_message("  strict-verify = confirm all commands before they are run")
                    user_message("  no-verify = don't confirm commands before they are run")
                    show_hint = False
                    continue
                elif not handle_cl_flag(cmd[0]):
                    user_message("That is not a valid command.")
                    continue
                history.append("--" + cmd[0])
            show_hint = False
            show_features = True
            print()
        user_message("You can replicate this configuration with: ./setup.py {}"
                     .format(" ".join(history)))
    if verify:
        if strict_verify:
            user_message("You will be asked to confirm all commands "
                         "before they are run.")
        else:
            user_message("You will be asked to confirm all potentially "
                         "dangerous commands before they are run.")
        user_message("When a command prompt appears, press RET.")
    if verify or interactive:
        wait_for_user("Press RET to continue.")
    for feature in feature_list:
        if feature["enabled"]:
            feature["action"]()


if __name__ == "__main__":
    setup()
