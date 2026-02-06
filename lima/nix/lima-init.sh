#!/usr/bin/env bash
set -euo pipefail

LIMA_CIDATA_MNT="@LIMA_CIDATA_MNT@"

echo "attempting to fetch configuration from LIMA user data..."

if [ ! -f "$LIMA_CIDATA_MNT"/lima.env ]; then
  echo "lima.env not found"
  exit 2
fi

# Can't just source lima.env because values might have spaces in them
while read -r line; do export "$line"; done <"$LIMA_CIDATA_MNT"/lima.env

export PATH=@PATH_DEPS@:$PATH

# Create user
LIMA_CIDATA_HOMEDIR="/home/$LIMA_CIDATA_USER"
id -u "$LIMA_CIDATA_USER" >/dev/null 2>&1 || useradd --home-dir "$LIMA_CIDATA_HOMEDIR" --create-home --uid "$LIMA_CIDATA_UID" "$LIMA_CIDATA_USER"

# Add user to groups
usermod -a -G wheel "$LIMA_CIDATA_USER"
usermod -a -G users "$LIMA_CIDATA_USER"

# Ensure /bin/bash exists
ln -fs /run/current-system/sw/bin/bash /bin/bash

# Set up SSH authorized_keys
LIMA_CIDATA_SSHDIR="$LIMA_CIDATA_HOMEDIR"/.ssh
mkdir -p -m 700 "$LIMA_CIDATA_SSHDIR"
awk '/ssh-authorized-keys/ {flag=1; next} /^ *$/ {flag=0} flag {sub(/^ +- /, ""); gsub("\"", ""); print $0}' \
  "$LIMA_CIDATA_MNT"/user-data >"$LIMA_CIDATA_SSHDIR"/authorized_keys
LIMA_CIDATA_GID=$(id -g "$LIMA_CIDATA_USER")
chown -R "$LIMA_CIDATA_UID:$LIMA_CIDATA_GID" "$LIMA_CIDATA_SSHDIR"
chmod 600 "$LIMA_CIDATA_SSHDIR"/authorized_keys

LIMA_SSH_KEYS_CONF=/etc/ssh/authorized_keys.d
mkdir -p -m 700 "$LIMA_SSH_KEYS_CONF"
cp "$LIMA_CIDATA_SSHDIR"/authorized_keys "$LIMA_SSH_KEYS_CONF/$LIMA_CIDATA_USER"

# Add mounts to /etc/fstab
sed -i '/#LIMA-START/,/#LIMA-END/d' /etc/fstab
echo "#LIMA-START" >> /etc/fstab
awk -f- "$LIMA_CIDATA_MNT"/user-data <<'EOF' >> /etc/fstab
/^mounts:/ {
    flag = 1
    next
}
/^[^:]*:/ {
    flag = 0
}
/^ *$/ {
    flag = 0
}
flag {
    sub(/^ *- \[/, "")
    sub(/"?\] *$/, "")
    gsub("\"?, \"?", "\t")
    print $0
}
EOF
echo "#LIMA-END" >> /etc/fstab

# Activate new mounts before running provisioning scripts
systemctl daemon-reload
systemctl restart local-fs.target

# Provision data files
if [ -d "$LIMA_CIDATA_MNT"/provision.data ]; then
  for f in "$LIMA_CIDATA_MNT"/provision.data/*; do
    filename=$(basename "$f")
    path_var="LIMA_CIDATA_DATAFILE_${filename}_PATH"
    owner_var="LIMA_CIDATA_DATAFILE_${filename}_OWNER"
    permissions_var="LIMA_CIDATA_DATAFILE_${filename}_PERMISSIONS"
    overwrite_var="LIMA_CIDATA_DATAFILE_${filename}_OVERWRITE"
    path="${!path_var}"
    owner="${!owner_var}"
    permissions="${!permissions_var}"
    overwrite="${!overwrite_var}"
    if [ -z "$path" ]; then
      echo "No path for data file $filename, skipping"
      continue
    fi
    if [ -f "$path" ] && [ "$overwrite" != "true" ]; then
      echo "Data file $path already exists, skipping"
      continue
    fi
    echo "Provisioning data file $path"
    mkdir -p "$(dirname "$path")"
    cp "$f" "$path"
    if [ -n "$owner" ]; then
      chown "$owner" "$path"
    fi
    if [ -n "$permissions" ]; then
      chmod "$permissions" "$path"
    fi
  done
fi

# Run system provisioning scripts
if [ -d "$LIMA_CIDATA_MNT"/provision.system ]; then
  for f in "$LIMA_CIDATA_MNT"/provision.system/*; do
    echo "Executing $f"
    if ! "$f"; then
      echo "Failed to execute $f"
    fi
  done
fi

# Run user provisioning scripts
USER_SCRIPT="$LIMA_CIDATA_HOMEDIR/.lima-user-script"
if [ -d "$LIMA_CIDATA_MNT"/provision.user ]; then
  if [ ! -f /sbin/openrc-run ]; then
    until [ -e "/run/user/$LIMA_CIDATA_UID/systemd/private" ]; do sleep 3; done
  fi
  params=$(grep -o '^PARAM_[^=]*' "$LIMA_CIDATA_MNT"/param.env | paste -sd ,)
  for f in "$LIMA_CIDATA_MNT"/provision.user/*; do
    echo "Executing $f (as user $LIMA_CIDATA_USER)"
    cp "$f" "$USER_SCRIPT"
    chown "$LIMA_CIDATA_USER" "$USER_SCRIPT"
    chmod 755 "$USER_SCRIPT"
    if ! /run/wrappers/bin/sudo -iu "$LIMA_CIDATA_USER" "--preserve-env=$params" "XDG_RUNTIME_DIR=/run/user/$LIMA_CIDATA_UID" "$USER_SCRIPT"; then
      echo "Failed to execute $f (as user $LIMA_CIDATA_USER)"
    fi
    rm "$USER_SCRIPT"
  done
fi

cp "$LIMA_CIDATA_MNT"/meta-data /run/lima-ssh-ready
cp "$LIMA_CIDATA_MNT"/meta-data /run/lima-boot-done
