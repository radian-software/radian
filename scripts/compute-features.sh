#!/bin/bash

### Utility function ###

# Usage: contains <element> <array> <var>
# Returns zero iff <element> is in <array>. Sets $<var> to the index of
# <element> in <array>, or unsets it if <element> is not in <array>.
# Note that <array> should be passed as array[@], not "${array[@]}".
# See http://stackoverflow.com/a/4017175/3538165
index_of() {
    local array=("${!2}")
    local index
    for index in "${!array[@]}"; do
        if [[ ${array[$index]} == $1 ]]; then
            eval "$3=\"$index\""
            return 0
        fi
    done
    declare $3=
    return 1
}

### Parse quantifier ###

case $1 in
    include | only ) default=false ;;
    exclude | except | but | "" ) default=true ;;
    * ) echo "[compute-features] Invalid qualifier '$1'. Must be one of [include only exclude except but]."
        exit 1 ;;
esac

if [[ $default == true ]]; then
    nondefault=false
else
    nondefault=true
fi

### Declare variables ###

# This isn't actually necessary, but is nice to have as a reference.
declare -a names
declare -a states
declare -a desc_lists
declare -a anc_lists

### Initialize names, states, and anc_lists ###

for i in "${!specs[@]}"; do
    spec=${specs[$i]}
    IFS=" " read -ra spec_array <<< "$spec"
    names[$i]=${spec_array[0]}
    states[$i]=$default
    if (( ${#spec_array[@]} >= 3 )); then
        anc_lists[$i]=${spec#* * }
    else
        anc_lists[$i]=
    fi
done

### Propagate dependencies through desc_lists and anc_lists ###

changed=true
while [[ $changed == true ]]; do
    changed=false
    for list_idx in "${!anc_lists[@]}"; do
        anc_list=${anc_lists[$list_idx]}
        IFS=" " read -ra ancs <<< "$anc_list"
        for anc_idx in "${!ancs[@]}"; do
            anc=${ancs[$anc_idx]}
            index_of "$anc" names[@] anc_list_idx
            # Add each ancestor of this feature's ancestors to the ancestors
            # of this feature, if necessary.
            anc_anc_list=${anc_lists[$anc_list_idx]}
            IFS=" " read -ra anc_ancs <<< "$anc_anc_list"
            for anc_anc_idx in "${!anc_ancs[@]}"; do
                anc_anc=${anc_ancs[$anc_anc_idx]}
                if ! (echo "$anc_list" | egrep -q "\\b$anc_anc\\b"); then
                    anc_lists[$list_idx]="$anc_list $anc_anc"
                    changed=true
                fi
            done
            # Add this feature to the descendents of each of its ancestors,
            # if necessary.
            anc_desc_list=${desc_lists[$anc_list_idx]}
            feat=${names[$list_idx]}
            if ! (echo "$anc_desc_list" | egrep -q "\\b$feat\\b"); then
                desc_lists[$anc_list_idx]="$anc_desc_list $feat"
                changed=true
            fi
        done
    done
done

### Parse arguments ###

if (( $# >= 2 )); then
    for feature in "${@:2}"; do
        if index_of "$feature" names[@] list_idx; then
            if [[ ${states[$list_idx]} == $default ]]; then
                if [[ $default == true ]]; then
                    echo "[compute-features] Disabling $feature."
                else
                    echo "[compute-features] Enabling $feature."
                fi
                states[$list_idx]=$nondefault
            fi
            if [[ $default == true ]]; then
                relative_list="${desc_lists[$list_idx]}"
            else
                relative_list="${anc_lists[$list_idx]}"
            fi
            IFS=" " read -ra relatives <<< "$relative_list"
            for relative in "${relatives[@]}"; do
                index_of "$relative" names[@] relative_idx
                if [[ ${states[$relative_idx]} == $default ]]; then
                    if [[ $default == true ]]; then
                        echo "[compute-features] $relative requires $feature. Also disabling $relative."
                    else
                        echo "[compute-features] $feature requires $relative. Also enabling $relative."
                    fi
                    states[$relative_idx]=$nondefault
                fi
            done
        else
            echo "[compute-features] Invalid feature '$feature'. Must be one of [${names[@]}]."
            exit 1
        fi
    done
fi

### Utility function ###

# Returns zero iff the feature $1 is enabled.
feature() {
    index_of "$1" names[@] i
    if [[ ${states[$i]} == true ]]; then
        return 0
    elif [[ ${states[$i]} == false ]]; then
        return 1
    else
        echo "[compute-features] Assertion error."
        exit 1
    fi
}

### Check for all features being disabled or enabled ###

no_features=true
all_features=true
for feature in "${names[@]}"; do
    if feature $feature; then
        no_features=false
    else
        all_features=false
    fi
done

if [[ $no_features == true ]]; then
    echo "[compute-features] All features have been disabled."
    echo "[compute-features] Aborting."
    exit 1
fi

echo "[compute-features] The following features will be set up:"
for feature in "${names[@]}"; do
    if feature $feature; then
        echo "* ${names[$i]}"
    fi
done

if [[ $all_features == true ]]; then
    echo "[compute-features] All features have been enabled."
else
    echo "[compute-features] The following features will not be set up:"
    for feature in "${names[@]}"; do
        if ! feature $feature; then
            echo "* ${names[$i]}"
        fi
    done
fi

echo -n "[compute-features] Is this OK? (y/n) "
read answer
if ! (echo "$answer" | egrep -qi "^y"); then
    echo "[compute-features] Aborting."
    exit 1
fi
