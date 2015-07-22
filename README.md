Pegasus
=======

Repository manager for Sagittarius Scheme.

How to install
==============

Installing Pegasus requires Pegasus itself. So for the very first time,
you need to clone the GitHub repository into your convenient place. Then
run `install.sh` (POSIX environment) or `install.bat` (Windows environment).
The followings are the complete commands:

For POSIX environment: 
```bash
$ git clone https://github.com/ktakashi/pegasus-library.git
$ cd pegasus-library
$ ./install.sh
```

For Windows environment: 
```bat
> git clone https://github.com/ktakashi/pegasus-library.git
> cd pegasus-library
> .\install.bat
```

Only for POSIX:

If you want to install Pegasus not the default location (`/usr/local`), then
you can specify `--prefix` option to change. For more information, run
`install.sh help`.


How to add package
==================

Pegasus manages packages on GitHub with meta files. Adding a meta file
to the GitHub repository requries Pull Request of the properly structured
meta file. The meta file must be located in the `formula` directory of the
GitHub repository. So whenever you want to add a package, then the following
steps are required:

1. Clone/synchronise the [repository](https://github.com/ktakashi/pegasus.git)
2. Add your meta file into `formula` directory
3. Test the meta file works
4. Send Pull Request.

Testing can be done by `-u` or `-i` option of `pegasus` command. The following
command shows how to do it:

```bash
$ pegasus install foo -i formula/your-package.scm
```

or if you already pushed to your repository, then 

```bash
$ pegasus install foo -u https://server/your/formula.scm
```

A meta file must contain one S-expression which provides the 
information of target package. The structure off the S-expression must be 
the following:


### `(formula _meta-info_)`
where _meta-info_ must be the followings:

### `(description _description_)` : optional
_description_ must be a string. Describes the package.

### `(version _version_)`: required
_version_ must be a string. The version of the package.

Currently there is no sophisticated comparison/management between different
versions. So if you want to install 2 different versions, then users need to
choose either overwrite the one installed or quit installing.

### `(homepage :url _url_)`: optional
_url_ must be a string. Web URL of the project page.

### `(author :name _name_ :email _email_)`: optional
_name_ and _email_ must be strings.

### `(source :type _type_ :url _url_ :compression _compression_)`: required
_type_ must be either `tar` or `zip` symbol. 
_url_ must be a url. Its scheme must be one of the followings:

- http
- https
- file

If the _type_ is `tar` and it's compressed, then _compression_ is required.
Currently it only supports `gzip`.

This specifies the package archive file to install.

If the `file` scheme is used, then the package archive must be located
either, `.pegasus/work` or current directory.

### `(install (directories (_dir-spec_ ...)) ... (files _file_ ...))`: required
_dir-spec_ must be a list whose first element is a installing directory name.

_file_ must be a string which is a installing file name.

Specifies which file or directory to install.

If _dir-spec_ contains `:exclude` keyword followed by a list of filename 
which must be a string, then the specified files located in the directory
are ignored.

### `(tests (test :file _file_ :style _style_ :loadpath _path_) ...)`: optional
_file_ must be a string and indicates a test file.

_style_ must be a symbol of either `srfi-64` or `srfi-78`. This is a style
of which testing framework is used.

_path_ must be a string. This is added to load path when the test is executed.

Specifies tests. If the one of the test fails, then Pegasus either asks users
to install or not, or abort installation.

NOTE: both testing frameworks may have custom reporting style, however Pegasus
can't handle them. So make sure if you don't change reporting style.
