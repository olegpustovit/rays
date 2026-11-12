#!/bin/sh
# Assemble target/bundle/Rays.app from the release build (rays, libpxui and every non-system dylib they load), sign it, and copy it
# into $1 (the applications directory). CODESIGN_IDENTITY: a name or hash from `security find-identity -v -p codesigning`;
# empty picks "Developer ID Application", else "Apple Development", else signs ad hoc. Notarization is not done.
set -eu
dest=${1:?usage: bundle-macos.sh APPDIR}
cd "$(dirname "$0")/.."
app=target/bundle/Rays.app
fw=$app/Contents/Frameworks

id=${CODESIGN_IDENTITY:-}
if [ -z "$id" ]; then
    found=$(security find-identity -v -p codesigning | sed -n 's/^ *[0-9]*) \([0-9A-F]\{40\}\) "\(.*\)"$/\1 \2/p')
    for kind in "Developer ID Application" "Apple Development"; do
        id=$(echo "$found" | grep -F " $kind" | head -n 1 | cut -d' ' -f1) && [ -n "$id" ] && break
    done
    if [ -z "$id" ]; then
        echo "WARNING: no codesigning identity found: signing AD HOC; the app only runs on this machine" >&2
        id=-
    else
        echo "signing identity: $(echo "$found" | grep "^$id" | cut -d' ' -f2- | cut -d: -f1) ($id)"
    fi
fi

rm -rf "$app"
mkdir -p "$app/Contents/MacOS" "$app/Contents/Resources" "$fw"
cp packaging/Rays.icns "$app/Contents/Resources/Rays.icns"
cp target/release/rays "$app/Contents/MacOS/rays"
cp -L target/release/libpxui.1.dylib "$fw/libpxui.1.dylib"

# Copy a file's non-system dylibs into Frameworks (recursively) and point it at them through @rpath. The binary's rpath is the
# bundle's Frameworks; an @rpath dependency of a copied library is its sibling in the source directory ($2); the dev-tree rpaths (target/<profile>) are removed everywhere.
fix() {
    codesign --remove-signature "$1" 2>/dev/null || true   # install_name_tool would warn on every edit of a signed file
    for old in $(otool -l "$1" | awk '/cmd LC_RPATH/{f=1} f&&/ path /{print $2; f=0}'); do install_name_tool -delete_rpath "$old" "$1"; done
    for dep in $(otool -L "$1" | tail -n +2 | awk '{print $1}'); do
        case $dep in
        /usr/lib/* | /System/*) ;;
        /* | @rpath/*)
            base=${dep##*/}
            case $dep in /*) install_name_tool -change "$dep" "@rpath/$base" "$1"; src=$dep ;; *) src=${2:-}/$base ;; esac  # before the recursion: it reuses these variables
            if [ ! -e "$fw/$base" ]; then
                cp -L "$src" "$fw/$base"; chmod u+w "$fw/$base"
                install_name_tool -id "@rpath/$base" "$fw/$base"
                fix "$fw/$base" "${src%/*}"
            fi ;;
        *) echo "unexpected load path $dep in $1" >&2; exit 1 ;;
        esac
    done
}
fix "$app/Contents/MacOS/rays"
fix "$fw/libpxui.1.dylib"
install_name_tool -add_rpath @executable_path/../Frameworks "$app/Contents/MacOS/rays"

version=$(sed -n 's/^version = "\(.*\)"/\1/p' Cargo.toml | head -n 1)
cat > "$app/Contents/Info.plist" <<P
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>CFBundleName</key><string>Rays</string>
<key>CFBundleDisplayName</key><string>Rays</string>
<key>CFBundleIdentifier</key><string>dev.rays.Rays</string>
<key>CFBundleIconFile</key><string>Rays</string>
<key>CFBundleExecutable</key><string>rays</string>
<key>CFBundlePackageType</key><string>APPL</string>
<key>CFBundleVersion</key><string>$version</string>
<key>CFBundleShortVersionString</key><string>$version</string>
<key>LSMinimumSystemVersion</key><string>11.0</string>
<key>NSHighResolutionCapable</key><true/>
</dict></plist>
P

# Inner libraries first, then the app. Hardened runtime unless ad hoc (its libraries carry no team to validate against);
# a secure timestamp only for Developer ID.
flags=
if [ "$id" != - ]; then
    flags="--options runtime"
    case $(security find-identity -v -p codesigning | grep -F "$id") in *"Developer ID"*) flags="$flags --timestamp" ;; esac
fi
for lib in "$fw"/*.dylib; do codesign --force --sign "$id" $flags "$lib"; done
codesign --force --sign "$id" $flags "$app"
codesign --verify --deep --strict --verbose=2 "$app"
spctl -a -vv "$app" || echo "spctl rejected the app (expected without notarization or a trusted Developer ID)"

mkdir -p "$dest"
rm -rf "$dest/Rays.app"
ditto "$app" "$dest/Rays.app"
echo "installed $dest/Rays.app"
