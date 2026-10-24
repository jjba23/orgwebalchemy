#
# spec file for package orgwebalchemy
#
# Copyright (c) 2026 SUSE LLC and contributors
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#

# Scheme sources plus a bash launcher: there is no compiled binary to
# debug, so the empty -debugsource package is skipped.
%global debug_package %{nil}

Name:           orgwebalchemy
Version:        1.2.2
Release:        0
Summary:        Org-mode parser and HTML/Markdown renderer for Guile
License:        LGPL-3.0-or-later
URL:            https://codeberg.org/jjba23/orgwebalchemy
Source0:        %{name}-%{version}.tar.gz
BuildRequires:  pkgconfig(guile-3.0)
BuildRequires:  guile-lib
BuildRequires:  veritas
Requires:       guile
Requires:       guile-lib

# NOTE: guile-lib is not packaged in openSUSE yet.  Build it first
# (from a devel project if needed) before this package.
# veritas is produced by the same packaging batch; build it first.

%description
Orgwebalchemy parses Org-mode with a PEG grammar and renders it as HTML,
Markdown or SXML. It is meant for static site generators and other Guile
programs that want to read Org files.

%prep
%autosetup -n %{name}-%{version}

%build
# Nothing to build: the program is Scheme sources plus a launcher, and
# the sources are byte-compiled during %install so that they land in
# the interpreter's compiled-module directory.

%install
# Both directories are asked of Guile's pkg-config file rather than
# assumed, so the modules land where this distribution's Guile looks
# for them.
SITE_DIR="%{buildroot}$(pkg-config --variable=sitedir guile-3.0)"
CCACHE_DIR="%{buildroot}$(pkg-config --variable=siteccachedir guile-3.0)"

mkdir -p "$SITE_DIR/orgwebalchemy" "$CCACHE_DIR/orgwebalchemy"
cp -r src/orgwebalchemy/. "$SITE_DIR/orgwebalchemy/"

# Byte-compile every module so that loading it does not compile it
# again on first use.
find "$SITE_DIR/orgwebalchemy" -name '*.scm' | while read -r scm_file; do
  rel_path="${scm_file#$SITE_DIR/}"
  go_file="$CCACHE_DIR/${rel_path%.scm}.go"
  mkdir -p "$(dirname "$go_file")"
  guild compile -L "$SITE_DIR" -o "$go_file" "$scm_file"
done

%check
VERITAS_TEST_TYPE=unit VERITAS_RUN_BENCHMARKS=false guile -L ./src -L ./test -c '((@ (veritas runner) run-tests))'

%files
%license COPYING
%doc README.org
%dir %{_datadir}/guile
%dir %{_datadir}/guile/site
%dir %{_datadir}/guile/site/3.0
%{_datadir}/guile/site/3.0/orgwebalchemy
%dir %{_libdir}/guile
%dir %{_libdir}/guile/3.0
%dir %{_libdir}/guile/3.0/site-ccache
%{_libdir}/guile/3.0/site-ccache/orgwebalchemy

%changelog
