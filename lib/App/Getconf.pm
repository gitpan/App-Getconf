#!/usr/bin/perl

=head1 NAME

App::Getconf - singleton-like config store for command-line applications

=head1 SYNOPSIS

  # main.pl

  use App::Getconf qw{ :schema };
  use YAML qw{ LoadFile };

  App::Getconf->option_schema(
    help    => opt { type => 'flag',
                     help => "this message" },
    version => opt { type => 'flag',
                     help => "print version information" },
    verbose => opt { type => 'bool',
                     help => "be verbose" },
    session => schema(
      timeout => opt { type => 'int',    value => 50 },
      path    => opt { type => 'string', value => '/' },
    ),
    # ...
  );

  App::Getconf->cmdline(\@ARGV);
  App::Getconf->options(LoadFile('/etc/myapp.yaml'));

  if (App::Getconf->getopt->help) {
    print App::Getconf->help_message();
    exit 0;
  }

  # real code...

  #-------------------------------------------------
  # My/Module.pm

  package My::Module;

  use App::Getconf;

  sub do_something {
    my ($self, %args) = @_;

    my $opts = App::Getconf->getopt;

    if ($opts->verbose) {
      print "Entering function do_something()\n";
    }

    # ...
  }

=head1 DESCRIPTION

This module is yet another command line options parser. But not only.
Actually, it's an option container. It's a response to a question: after
parsing options (from command line and from config file), how do I pass them
down the function call stack?

There are two classic approaches. One utilizes global variables. This is not
that convenient, because introduces some names treated in special way (not
defined inside the current function). The other requires passing option
container as an argument to each and every function (you can't always tell in
advance that the function will never use the options on one hand, and API
changes are tedious on the other).

App::Getconf tries a different way, which is not entirely new: the inspiration
for this module was L<Log::Log4perl(3)>, which is Perl port of log4j Java
library. The idea is simple: you need a value accessible similarly to a global
variable, but declared locally.

=head1 ARCHITECTURE

App::Getconf consists of three different types of objects: option
containers, option views and option schema nodes.

Option container (App::Getconf instance) stores all the options that were set,
either from command line or from multi-level hash (e.g. loaded config file).

Option container needs to be initialized with option schema: list of allowed
options, along with their types (int, float, string, flag and so on). Such
schema is composed of nodes created with C<opt()> function or derivatives.

Option view (L<App::Getconf::View(3)> instance) is an interface to options
list. When option is requested, view does a "lookup" to find appropriate
option. For example, view C<$v> for I<proto.client> subsystem was created.
When C<< $v->get('timeout') >> was issued, the view will return value of the
first existing option: I<proto.client.timeout>, I<proto.timeout> or
I<timeout>. Of course there's also a possibility to omit this lookup.

App::Getconf creates a default option container. This default container is
used every time when semi-static method (see L</"Semi-Static Methods">
section) is called as static one. This is how App::Getconf provides a way of
accessing options globally. However, you are not limited to this default
container. You may create your own containers with their own option schema. Of
course you will need to pass them down the call stack.

=head2 Options Lifecycle

Option container needs a schema to tell which options are legal and which are
not. Defining schema is basically the first thing to do. Schema can also
contain initial values for some options.

Next go options defined in command line and in config file. Option container
can parse command line on its own, it just needs an array of arguments.

Two above steps are only to be done once, at the application start, possibly
as early as possible. Changing option values, however, is planned in the
future to be supported after initialization process, at run-time.

From now on, C<getopt()> method may be used in any part of application.

=head2 Schema Definition

Schema is simply a hashref that contains options. Each value is a node (actual
option or alias) or a sub-schema.

Full name of an option from sub-schema is I<$schema.$option>, where
C<${schema}> is the key, under which sub-schema was stored. Command line
option that sets such option is I<--$schema-$option>.

Schemas stored under greater depth are analogous.

Example of schema:

  help    => opt { type => 'flag', ... },
  version => opt { type => 'flag', ... },
  verbose => opt { type => 'bool', ... },
  session => {
    timeout => opt { type => 'int',    ... },
    path    => opt { type => 'string', ... },
    ''      => opt { type => 'string', ... },
  },
  # ...

This schema defines options I<help>, I<version>, I<verbose>,
I<session.timeout>, I<session.path> and just plain I<session>. The last one is
example of how to define option of the same name as sub-schema.

End-user can set these options using command line options, accordingly:
I<--help>, I<--version>, I<--verbose>/I<--no-verbose>,
I<--session-timeout=###>, I<--session-path=XXX> and I<--session=XXX>.

Basic way of creating node is using C<opt()> function, but there are few
shorthands, like C<opt_int()>, C<opt_flag()> and others. See
L</"Functions Defining Schema"> section for details.

Schema is also used, beside validating option correctness, for generating
message printed typically after issuing I<--help> option. Only options having
C<help> field are included in this message. Other options still may be set in
command line, but are not exposed to the user. They are meant mainly to be
specified with configuration file or with other means.

Order of options in autogenerated help message is lexicographic order. You may
provide the order by changing Perl's built-in anonymous hashref C<{}> to call
to function C<schema()>. Example:

  # ...
  session => schema(
    timeout => opt { type => 'int',    ... },
    path    => opt { type => 'string', ... },
    ''      => opt { type => 'string', ... },
  ),
  # ...

You may freely mix hashrefs and C<schema()> calls, at the same or different
nesting levels.

=cut

package App::Getconf;

#-----------------------------------------------------------------------------

use warnings;
use strict;

use base qw{Exporter};
use Carp;
use App::Getconf::View;
use Tie::IxHash;

our $VERSION = '0.01';

our @EXPORT_OK = qw(
  schema
  opt        opt_alias
  opt_flag   opt_bool
  opt_int    opt_float
  opt_string opt_path   opt_hostname
  opt_re     opt_sub    opt_enum
);

our %EXPORT_TAGS = (
  schema => [ 'schema', grep { /^opt/ } @EXPORT_OK ],
);

#-----------------------------------------------------------------------------

my $static = new App::Getconf();

#-----------------------------------------------------------------------------

=head1 MODULE API

Following methods are available:

=over

=cut

#-----------------------------------------------------------------------------

=item C<new(%opts)>

Constructor.

No options are used at the moment.

B<NOTE>: You don't need to use the constructor. You may (and typically would)
want to use App::Getconf's default container.

=cut

sub new {
  my ($class, %opts) = @_;

  my $self = bless {
    schema  => undef,
    aliases => undef,
    options => undef,
    args    => undef,
    help    => {
      message => undef,
      order   => undef,
    },
    # each getopt() will return 
    getopt_cache  => {},
  }, $class;

  return $self;
}

#-----------------------------------------------------------------------------

=back

=head2 Semi-Static Methods

Methods from this section can be called as instance methods, when you have
created own instance of C<App::Getconf>, or as static methods, when they
operate on default instance of C<App::Getconf>. Typically you would use the
latter strategy, as passing option container down the function call stack is
somewhat troublesome.

=over

=cut

#-----------------------------------------------------------------------------

=item C<option_schema($schema_description)>

=item C<< option_schema(key => value, key => value, ...) >>

Set expected schema for the options. Schema may be either a hashref (Perl's
ordinary or created using C<schema()> function) or a list of key/value pairs.
The latter form has the same result as passing the list to C<schema()> first,
i.e., the options order will be preserved.

=cut

sub option_schema {
  my ($self, @args) = @_;

  my $schema = (@args == 1) ? $args[0] : schema(@args);;

  $self = $static unless ref $self; # static call or non-static?

  my @schema = _flatten($schema, "");
  $self->{schema} = {};
  $self->{aliases} = {};
  $self->{help}{order} = [];
  for my $opt (@schema) {
    # chomp leading and trailing periods from option name
    $opt->{name} =~ s/^\.|\.$//g;

    if ($opt->{opt}->alias) {
      # alias option

      $self->{aliases}{ $opt->{name} } = $opt->{opt};

    } else {
      # normal (non-alias) option

      $self->{schema}{ $opt->{name} } = $opt->{opt};
      # remember the order of messages
      if ($opt->{opt}->help) {
        push @{ $self->{help}{order} }, $opt->{name};
      }

      # set initial value if it's defined or it's a flag
      if ((exists $opt->{opt}{value} || $opt->{opt}->type eq 'flag') &&
          !exists $self->{options}{ $opt->{name} }) {
        $self->{options} ||= {};
        if ($opt->{opt}->type eq 'flag') {
          $self->{options}{ $opt->{name} } = 0;
        } else {
          $self->set_verify($opt->{opt}{value}, $opt->{name});
        }
      }
    }
  }

  # NOTE: this can't be moved to inside the previous loop, because there could
  # be an alias processed earlier than the option it points to
  for my $name (sort keys %{ $self->{aliases} }) {
    my $dest = $self->{aliases}{$name}->alias;

    # option can't be an alias and non-alias at the same time
    if ($self->{aliases}{$dest}) {
      croak "Alias \"$name\" points to another alias called \"$dest\"";
    } elsif (not $self->{schema}{$dest}) {
      croak "Alias \"$name\" points to a non-existent option \"$dest\"";
    }
  }

  # TODO: recalculate options
}

# function flattens schema hashref tree to a flat hash, where option names are
# separated by "."
#   * $root - root of schema hashref tree to convert (recursively)
#   * $path - path so far (should be empty string initially; intended for
#             recursive call)
# outcome is a hash with two fields:
#   * "name" contains full option path; it includes leading period and may
#     contain trailing period, so it requires small amount of postprocessing
sub _flatten {
  my ($root, $path) = @_;

  my @opts = eval { tied(%$root)->isa("Tie::IxHash") } ?
               keys %$root :
               sort keys %$root;
  my @result;
  for my $o (@opts) {
    if (ref $root->{$o} eq "App::Getconf::Opt") {
      push @result, { name => "$path.$o", opt => $root->{$o} };
    } elsif (ref $root->{$o} eq 'HASH') {
      # XXX: don't try $root->{$o}{""}, it will be collected in the recursive
      # _flatten() call (note that this may leave trailing period for this
      # option)
      push @result, _flatten($root->{$o}, "$path.$o");
    }
  }
  return @result;
}

#-----------------------------------------------------------------------------

=item C<help_message(%options)>

Return message printed when I<--help> (or similar) option was passed. Message
will be C<\n>-terminated.

Typical usage:

  if (App::Getconf->getopt->help) {
    print App::Getconf->help_message(
      screen   => 130,
      synopsis => "%0 [ options ] file ...",
    );
    exit 0;
  }

Supported options:

=over

=item C<screen> (default: 80)

Screen width, in columns.

=item C<arg0> (default: C<$0> with path stripped)

Name of the program. Usually C<$0> or a derivative.

=item C<synopsis> (default: C<%0 [ options ... ]>)

Short call summary. Any occurrence of C<%0> will be replaced with content of
C<arg0> option.

Synopsis may be also a multiline string or an array of single-line strings.

=item C<header>

=item C<description>

=item C<footer>

Three additional text fields: before synopsis, after synopsis but before
options list, after options list.

Text will be re-wrapped to fit on a terminal of C<screen> width. Empty lines
will be treated as paragraph separators, but single newline characters will
not be preserved.

Any occurrence of C<%0> will be replaced with content of C<arg0> option.

=item C<option_indent> (default: 2)

=item C<description_indent> (default: 6)

Indenting for option header ("--option" with parameter specification, if any)
and for option description.

=back

=cut

sub help_message {
  my ($self, %opts) = @_;

  $self = $static unless ref $self; # static call or non-static?

  $opts{screen}   ||= 80;
  $opts{arg0}     ||= (split m[/], $0)[-1];
  $opts{synopsis} ||= "%0 [ options ... ]";

  $opts{option_indent}      ||= 2;
  $opts{description_indent} ||= 6;

  # $opts{header}      ||= undef;
  # $opts{description} ||= undef;
  # $opts{footer}      ||= undef;

  my $help = "";
  my $line;
  my %format_markers;

  #---------------------------------------------------------
  # synopsis

  if ($opts{header}) {
    $line = _reformat($opts{header}, $opts{screen});
    $line =~ s/%0/$opts{arg0}/g;

    $help .= $line;
    $help .= "\n"; # additional empty line
  }

  if (ref $opts{synopsis} eq 'ARRAY') {
    $line = join "\n", @{ $opts{synopsis} };
  } else {
    $line = $opts{synopsis};
  }
  $line =~ s/%0/$opts{arg0}/g;

  $line =~ s/\s+$//; # strip leading spaces

  if ($line =~ /\n./) {
    # multiline synopsis
    $format_markers{multiline_synopsis} = 1;

    $line =~ s/^[ \t]*/  /mg; # uniform indentation
    $help .= sprintf "Usage:\n%s\n", $line;

  } else {
    # single line synopsis

    $line =~ s/^\s+//; # strip leading spaces
    if (length($line) < $opts{screen} - 1 - length("Usage: ")) {
      $help .= sprintf "Usage: %s\n", $line;
    } else {
      $format_markers{multiline_synopsis} = 1;
      $help .= sprintf "Usage:\n%s\n", $line;
    }

  }

  if ($opts{description}) {
    $line = _reformat($opts{description}, $opts{screen});
    $line =~ s/%0/$opts{arg0}/g;

    $help .= "\n";
    $help .= $line;

    $format_markers{multiline_synopsis} = 1;
  }

  #---------------------------------------------------------
  # options

  if ($self->{help}{order} && @{ $self->{help}{order} }) {
    $line = "Options available:\n";

    for my $opt (@{ $self->{help}{order} }) {
      my $dash_opt = (length $opt > 1) ? "--$opt" : "-$opt";
      $dash_opt =~ tr/./-/;

      my $init_val = "";
      if ($self->{schema}{$opt}->has_value) {
        $init_val = $self->{schema}{$opt}->value;
        $init_val = "<undef>" if not defined $init_val;
        $init_val = " (initially: $init_val)";
      }

      # option header (indented "--option")
      # TODO: aliases
      if ($self->{schema}{$opt}->type eq 'flag') {
        $line .= sprintf "%*s%s\n", $opts{option_indent}, "", $dash_opt;
      } elsif ($self->{schema}{$opt}->type eq 'bool') {
        my $neg_dash_opt = "--no-$opt";
        $neg_dash_opt =~ tr/./-/;
        $line .= sprintf "%*s%s, %s\n",
                         $opts{option_indent}, "",
                         ($self->{schema}{$opt}->value ?
                           ($neg_dash_opt, $dash_opt) :
                           ($dash_opt, $neg_dash_opt));
      } elsif ($self->{schema}{$opt}->has_default) {
        my $type = $self->{schema}{$opt}->type;
        if (ref $self->{schema}{$opt}{check} eq 'ARRAY') {
          # enums
          $type = join "|", @{ $self->{schema}{$opt}{check} };
        }
        $line .= sprintf "%*s%s, %s=%s%s\n",
                         $opts{option_indent}, "",
                         $dash_opt,
                         $dash_opt, $type,
                         $init_val;
      } else {
        my $type = $self->{schema}{$opt}->type;
        if (ref $self->{schema}{$opt}{check} eq 'ARRAY') {
          # enums
          $type = join "|", @{ $self->{schema}{$opt}{check} };
        }

        $line .= sprintf "%*s%s=%s%s\n",
                         $opts{option_indent}, "",
                         $dash_opt, $type,
                         $init_val;
      }

      $line .= _reformat(
        $self->{schema}{$opt}->help,
        $opts{screen}, $opts{description_indent}
      );
    }

    if (_nlines($line) > 16 || $format_markers{multiline_synopsis} ||
        $opts{header} || $opts{description}) {
      # additional empty line between synopsis and options description
      $help .= "\n";
    }

    $help .= $line;
    $format_markers{has_options} = 1;
  }

  if ($opts{footer}) {
    $line = _reformat($opts{footer}, $opts{screen});
    $line =~ s/%0/$opts{arg0}/g;

    $help .= "\n";
    $help .= $line;
  }

  #---------------------------------------------------------

  return $help;
}

sub _nlines {
  my ($str) = @_;

  my $nlines =()= ($str =~ /\n/g);

  return $nlines;
}

# reformat a multiparagraph string to include maximum of ($width-1) characters
# per line, including indentation
sub _reformat {
  my ($str, $width, $indent) = @_;

  $indent ||= 0;

  my @result;

  $str =~ s/^\s+//;
  for my $para (split /\n\s*\n[ \t]*/, $str) {
    my $r = "";
    my $line = "";
    for my $w (split /\s+/, $para) {
      if ($line eq "") {
        $line = (" " x $indent) . $w;
      } elsif (length($line) + 1 + length($w) < $width) {
        $line .= " " . $w;
      } else {
        $r .= $line . "\n";
        $line = (" " x $indent) . $w;
      }
    }
    $r .= $line . "\n";
    push @result, $r;
  }

  return join "\n", @result;
}

#-----------------------------------------------------------------------------

=item C<options($options)>

Set options read from configuration file (hashref).

Example usage:

  App::Getconf->options(YAML::LoadFile("/etc/myapp.yaml"));

=cut

sub options {
  my ($self, $options) = @_;

  $self = $static unless ref $self; # static call or non-static?
  $self->{options} ||= {};          # in case it was empty

  $self->set_verify($options);
}

#-----------------------------------------------------------------------------

=item C<cmdline($arguments)>

Set options based on command line arguments (arrayref). If C<$arguments> was
not specified, C<@ARGV> is used.

Method returns list of messages (single line, no C<\n> at end) for errors that
were found, naturally empty if nothing was found.

Arguments that were not options can be retrieved using C<args()> method.

Example usage:

  App::Getconf->cmdline(\@ARGV);
  # the same: App::Getconf->cmdline();
  for my $arg (App::Getconf->args()) {
    # ...
  }

=cut

sub cmdline {
  my ($self, $arguments) = @_;

  $self = $static unless ref $self; # static call or non-static?

  my @args;
  # replace short options with their double-dash versions
  for my $arg (@{ $arguments || \@ARGV }) {
    if ($arg =~ /^-([a-zA-Z0-9]+)$/) {
      my $candidates = $1;
      my @unrecognized;
      for my $c (split //, $candidates) {
        if ($self->{schema}{$c} || $self->{aliases}{$c}) {
          push @args, join "", ("-", @unrecognized) if @unrecognized;
          @unrecognized = ();
          push @args, "--$c";
        } else {
          push @unrecognized, $c;
        }
      }
      push @args, join "", ("-", @unrecognized) if @unrecognized;
    } else {
      push @args, $arg;
    }
  }

  my @left;
  my @errors;

  OPTION:
  for (my $i = 0; $i < @args; ++$i) {
    if ($args[$i] =~ /^--([a-zA-Z0-9-]+)=(.*)$/) {
      # option with a value
      my ($name, $value) = ($1, $2);

      # try resolving alias first
      my $orig_name = $name;
      if ($self->{aliases}{ _sw_to_opt($name) }) {
        $name = $self->{aliases}{ _sw_to_opt($name) }->alias;
      }

      if (not $self->{schema}{ _sw_to_opt($name) }) {
        push @errors, {
          option => "--$orig_name",
          cause => "unknown option",
        };
        next OPTION;
      }

      if ($self->{schema}{ _sw_to_opt($name) }->storage eq 'HASH') {
        my ($k, $v) = split /=/, $value, 2;
        if (not defined $v) {
          push @errors, {
            option => "--$orig_name",
            cause => "value \"$value\" is not a keyspec",
          };
          next OPTION;
        }
        $self->set_verify({ $k => $v }, _sw_to_opt($name));
      } else {
        $self->set_verify($value, _sw_to_opt($name));
      }

    } elsif ($args[$i] =~ /^--((no-)?([a-zA-Z0-9-]+))$/) {
      # generic option (could have a value, but this is to be checked)
      my ($full_name, $negated, $name) = ($1, $2, $3);
      my $value;

      # try resolving alias first
      my $orig_full_name = $full_name;
      my $orig_name = $name;
      if ($self->{aliases}{ _sw_to_opt($full_name) }) {
        $full_name = $self->{aliases}{ _sw_to_opt($full_name) }->alias;
      }
      if ($self->{aliases}{ _sw_to_opt($name) }) {
        $name = $self->{aliases}{ _sw_to_opt($name) }->alias;
      }

      my $node = $self->{schema}{ _sw_to_opt($full_name) };
      if ($node) {
        if ($node->type eq 'bool') {
          # bool option doesn't use arguments

          $self->set_verify(1, _sw_to_opt($full_name));

        } elsif ($node->type eq 'flag') {
          # flag doesn't use arguments

          # NOTE: flag is the only case of option set without using
          # set_verify() method
          # FIXME: this renders flag options to be non-settable with
          # set_verify(); is this bad or "don't care"?
          $self->{options}{ _sw_to_opt($full_name) } += 1;

        } elsif ($node->has_default) {
          # option with default argument

          $self->set_verify($node->default, _sw_to_opt($full_name));

        } else {
          # non-bool option uses an argument

          if ($i < @args - 1) {
            $i += 1;
            $value = $args[$i];
          } else {
            # missing argument
            push @errors, {
              option => "--$orig_full_name",
              cause => "missing argument",
            };
            next OPTION; # actually, this could be `last OPTION;'
          }

          if ($self->{schema}{ _sw_to_opt($name) }->storage eq 'HASH') {
            my ($k, $v) = split /=/, $value, 2;
            if (not defined $v) {
              push @errors, {
                option => "--$orig_name",
                cause => "value \"$value\" is not a keyspec",
              };
              next OPTION;
            }
            $self->set_verify({ $k => $v }, _sw_to_opt($name));
          } else {
            $self->set_verify($value, _sw_to_opt($name));
          }

        }
      } elsif ($negated && $self->{schema}{ _sw_to_opt($name) } &&
               $self->{schema}{ _sw_to_opt($name) }->type eq 'bool') {
        # bool option, but negated
        $self->set_verify(0, _sw_to_opt($name));
      } else {
        # not a known option, not a negated known bool option (negation for
        # non-bool options has no sense)
        push @errors, {
          option => "--$orig_full_name",
          cause => "unknown option",
        };
        next OPTION; # actually, this could be `last OPTION;'
      }

    } elsif ($args[$i] eq '--') {

      push @left, @args[$i + 1 .. @args - 1];
      last OPTION;

    } elsif ($args[$i] =~ /^-(.*)/) {

      push @errors, {
        option => "-$1",
        cause => "unknown option",
      };
      next OPTION;

    } else {

      push @left, $args[$i];

    }
  }

  $self->{args} = \@left;

  if (@errors) {
    return map { "$_->{option}: $_->{cause}" } @errors;
  } else {
    return;
  }
}

sub _sw_to_opt {
  my ($name) = @_;

  $name =~ tr/-/./;
  return $name;
}

#-----------------------------------------------------------------------------

=item C<set_verify($data)>

=item C<set_verify($data, $path)>

Set value(s) with verification against schema. If C<$path> was specified,
options start with this prefix. If values were verified successfully, they are
saved in internal storage.

B<NOTE>: This is a semi-internal API.

=cut

sub set_verify {
  my ($self, $data, $path) = @_;

  $self = $static unless ref $self; # static call or non-static?

  $path ||= "";
  my $schema = $self->{schema};
  my $store  = ($self->{options} ||= {});

  my $datum_type = ref $data;

  # this is an option, but there's no corresponding schema node
  if ($datum_type ne 'HASH' && !$schema->{$path}) {
    $datum_type = lc($datum_type || 'scalar');
    croak "Unexpected $datum_type option ($path)";
  }

  # simple case: data is a scalar
  if ($datum_type eq '') {
    # scalar can be stored in a scalar or in an array

    if ($schema->{$path}->storage eq '') {
      $store->{$path} = $schema->{$path}->check($data, $path);
    } elsif ($schema->{$path}->storage eq 'ARRAY') {
      $store->{$path} ||= [];
      push @{ $store->{$path}}, $schema->{$path}->check($data, $path);
    } elsif ($schema->{$path}->storage eq 'HASH') {
      croak "Scalar option found where hash was expected ($path)";
    }

    return;
  }

  # simple case: data is an array
  if ($datum_type eq 'ARRAY') {
    # array can only be stored in an array

    if ($schema->{$path}->storage ne 'ARRAY') {
      my $type = lc($schema->{$path}->storage || 'scalar');
      croak "Array option found where $type was expected ($path)";
    }

    for my $e (@$data) {
      $store->{$path} ||= [];
      push @{ $store->{$path} }, $schema->{$path}->check($e, $path);
    }

    return;
  }

  # more complex case: data is a hash

  # if no corresponding node in schema, just go deeper
  # if there is corresponding node, but it's not a hash, just go deeper
  if (!$schema->{$path} || $schema->{$path}->storage ne 'HASH') {
    for my $o (keys %$data) {
      my $new_path = "$path.$o";
      $new_path =~ s/^\.|\.$//g;

      $self->set_verify($data->{$o}, $new_path);
    }

    return;
  }

  # it's sure that $schema->{$path} exists and it's storage type is HASH
  # also, this option's type is hash

  $store->{$path} ||= {};
  for my $k (keys %$data) {
    $store->{$path}{$k} = $schema->{$path}->check($data->{$k}, $path);
  }
}

#-----------------------------------------------------------------------------

=item C<args()>

Retrieve non-option arguments (e.g. everything after "--") passed from command
line.

Values returned by this method are set by C<cmdline()> method.

=cut

sub args {
  my ($self) = @_;

  $self = $static unless ref $self; # static call or non-static?

  return @{ $self->{args} };
}

#-----------------------------------------------------------------------------

=item C<getopt($package)>

Retrieve a view of options (L<App::Getconf::View(3)>) appropriate for
package or subsystem called C<$package>.

If C<$package> was not provided, caller's package name is used.

C<$package> sets option search path. See C<new()>, C<prefix> option
description in L<App::Getconf::View(3)> for details.

Typical usage:

  sub foo {
    my (@args) = @_;

    my $opts = App::Getconf->getopt(__PACKAGE__);

    if ($opts->ssl) {
      # ...

=cut

sub getopt {
  my ($self, $package) = @_;

  $self = $static unless ref $self; # static call or non-static?
  if (not defined $package) {
    $package = caller;
    if (!defined $package || $package eq 'main') {
      $package = '';
    }
  }

  $package =~ s{/|::}{.}g;
  $package = lc $package;

  if (not $self->{getopt_cache}{$package}) {
    $self->{getopt_cache}{$package} = new App::Getconf::View(
      prefix  => $package,
      options => $self->{options},
      schema  => $self->{schema},
    );
  }

  return $self->{getopt_cache}{$package};
}

#-----------------------------------------------------------------------------

=back

=cut

#-----------------------------------------------------------------------------

=head2 Functions Defining Schema

=over

=cut

#-----------------------------------------------------------------------------

=item C<< schema(key => value, key => value, ...) >>

Create a hashref from key/value pairs. The resulting hash is tied to
L<Tie::IxHash(3)>, so the order of keys is preserved.

Main use is for defining order of options in I<--help> message, otherwise it
acts just like anonymous hashref creation (C<< { key => value, ... } >>).

=cut

sub schema {
  my (@args) = @_;

  tie my %h, 'Tie::IxHash';
  %h = @args;

  return \%h;
}

#-----------------------------------------------------------------------------

=item C<opt($data)>

Generic option specification.

Possible data:

  opt {
    type    => 'flag' | 'bool' | 'int' | 'float' | 'string',
    check   => qr// | sub {} | ["enum", "value", ...],
    storage => undef | \$foo | [] | {},
    help    => "message displayed on --help",
    value   => "initial value",
    default => "default value",
  }

If type is not specified, the option is treated as a string.

Check is for verifying correctness of specified option. It may be a regexp,
callback function (it gets the value to check as a first argument and in C<$_>
variable) or list of possible string values.

Types of options:

=over

=item C<flag>

Simple option, like I<--help> or I<--version>. Flag's value tells how many
times it was encountered.

=item C<bool>

ON/OFF option. May be turned on (I<--verbose>) or off (I<--no-verbose>).

=item C<int>

Option containing an integer.

=item C<float>

Option containing a floating point number.

=item C<string>

Option containing a string. This is the default.

=back

Storage tells if the option is a single-value (default), multi-value
accumulator (e.g. may be specified in command line multiple times, and the
option arguments will be stored in an array) or multi-value hash accumulator
(similar, but option argument is specified as C<key=value>, and the value part
is validated). Note that this specify only type of storage, not the actual
container.

B<NOTE>: Don't specify option with a hash storage and that has sub-options
(see L</"Schema Definition">). Verification can't tell whether the value is
meant for the hash under this option or for one of its sub-options.

Presence of C<help> key indicates that this option should be exposed to
end-users in I<--help> message. Options lacking this key will be skipped (but
stil honoured by App::Getconf).

Except for flags (I<--help>) and bool (I<--no-verbose>) options, the rest of
types require an argument. It may be specified as I<--timeout=120> or as
I<--timeout 120>. This requirement may be loosened by providing
C<default> value. This way end-user may just provide I<--timeout> option, and
the argument to the option is taken from default. (Of course, only
I<--timeout=120> form is supported if the argument needs to be provided.)

Initial value (C<value> key) is the value set for the option just after
defining schema. It may or may not be changed with command line options (which
is different from C<default>, for which the option still needs to be
specified).

Initial and default values are both subject to check that was specified, if
any.

Help message will not retain any formatting, all whitespaces are converted to
single space (empty lines are squeezed to single empty line). On the other
hand, the message will be pretty wrapped and indented, while you don't need to
worry about formatting the string if it is longer and broken to separate lines
in your source code, so I think it's a good trade-off.

=cut

sub opt($) {
  my ($data) = @_;

  my $type    = $data->{type} || "string";
  my $check   = $data->{check};
  my $storage = $data->{storage};
  my $help    = $data->{help};
  my $value   = $data->{value};   # not necessary, but kept for convention
  my $default = $data->{default}; # not necessary, but kept for convention

  if (ref $storage) {
    # make sure the store is not a reference to something outside of this
    # function
    if (ref $storage eq 'ARRAY') {
      $storage = 'ARRAY';
    } elsif (ref $storage eq 'HASH') {
      $storage = 'HASH';
    } elsif (ref $storage eq 'SCALAR') {
      $storage = '';
    } # TODO: else die?
  } else {
    $storage = '';
  }

  my $opt = bless {
    type    => $type,
    check   => $check,
    storage => $storage,
    help    => $help,
  }, 'App::Getconf::Opt';
  # XXX: this way undefs are possible to represent as undefs
  $opt->{value}   = $opt->check($value)   if exists $data->{value};
  $opt->{default} = $opt->check($default) if exists $data->{default};

  return $opt;
}

=item C<opt_alias($option)>

Create an alias for C<$option>. Note that aliases are purely for command line.
L<App::Getconf::View(3)> and C<options()> method don't honour them.

Aliases may only point to non-alias options.

=cut

sub opt_alias($) {
  my ($dest_option) = @_;

  return bless { alias => $dest_option }, 'App::Getconf::Opt';
}

=item C<opt_flag()>

Flag option (like I<--help>, I<--verbose> or I<--debug>).

=cut

sub opt_flag() {
  return opt { type => 'flag' };
}

=item C<opt_bool()>

Boolean option (like I<--recursive>). Such option gets its counterpart
called I<--no-${option}> (mentioned I<--recursive> gets I<--no-recursive>).

=cut

sub opt_bool() {
  return opt { type => 'bool' };
}

=item C<opt_int()>

Integer option (I<--retries=3>).

=cut

sub opt_int() {
  return opt { type => 'int' };
}

=item C<opt_float()>

Option specifying a floating point number.

=cut

sub opt_float() {
  return opt { type => 'float' };
}

=item C<opt_string()>

Option specifying a string.

=cut

sub opt_string() {
  return opt { type => 'string' };
}

=item C<opt_path()>

Option specifying a path in local filesystem.

=cut

sub opt_path() {
  # TODO: some checks on how this looks like
  #   * existing file
  #   * existing directory
  #   * non-existing file (directory exists)
  #   * Maasai?
  return opt { type => 'string' };
}

=item C<opt_hostname()>

Option specifying a hostname.

B<NOTE>: This doesn't check DNS for the hostname to exist. This only checks
hostname's syntactic correctness (and only to some degree).

=cut

sub opt_hostname() {
  return opt { check => qr/^[a-z0-9-]+(\.[a-z0-9-]+)*$/i };
}

=item C<opt_re(qr/.../)>

Option specifying a string, with check specified as regexp.

=cut

sub opt_re($) {
  my ($re) = @_;

  return opt { check => $re };
}

=item C<opt_sub(sub {...})>

=item C<opt_sub {...}>

Option specifying a string, with check specified as function (code ref).

Subroutine will have C<$_> set to value to check, and the value will be the
only argument (C<@_>) passed.

Subroutine should return C<TRUE> when option value should be accepted,
C<FALSE> otherwise.

=cut

sub opt_sub(&) {
  my ($sub) = @_;

  return opt { check => $sub };
}

=item C<opt_enum ["first", ...]>

Option specifying a string. The string must be one of the specified in the
array.

=cut

sub opt_enum($) {
  my ($choices) = @_;

  return opt { check => $choices };
}

#-----------------------------------------------------------------------------

=back

=cut

#-----------------------------------------------------------------------------

=head1 AUTHOR

Stanislaw Klekot, C<< <cpan at jarowit.net> >>

=head1 LICENSE AND COPYRIGHT

Copyright 2012 Stanislaw Klekot.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=head1 SEE ALSO

L<App::Getconf::View(3)>, L<Getopt::Long(3)>, L<Tie::IxHash(3)>.

=cut

#-----------------------------------------------------------------------------
#
# utility class that represents an option specification in schema
#
#-----------------------------------------------------------------------------

package App::Getconf::Opt;

use Carp;
# App::Getconf::View is supposed to be carp-silenced by the transitivity rule
our @CARP_NOT = qw{App::Getconf};

sub check {
  my ($self, $value, $optname) = @_;

  my $type  = $self->{type} || 'string';
  my $check = $self->{check};

  # string appended to die()
  my $optname_die_str = defined $optname ? " ($optname)" : "";

  eval {
    # convert warnings to errors
    local $SIG{__WARN__} = sub { die $@ };

    if ($type eq 'string') {
      $value = defined $value ? "$value" : undef;
    } elsif ($type eq 'int') {
      # TODO: better check
      $value = int(0 + $value);
    } elsif ($type eq 'float') {
      # TODO: better check
      $value = 0.0 + $value;
    } # TODO: how to check boolean?
    # XXX: flags are not supposed to be processed by this function
  };

  # on any warning, assume the data is not in correct format
  if ($@) {
    croak "Invalid value \"$value\" for type $type$optname_die_str";
  }
  if ($type eq 'flag') {
    croak "Flag can't have a value$optname_die_str";
  }

  # check for correctness

  if (not $self->{check}) {
    # no check, so everything is OK

    return $value;
  } elsif (ref $self->{check} eq 'CODE') {
    # check based on function

    if (do { local $_ = $value; $self->{check}->($_) }) {
      return $value;
    } else {
      croak "Value \"$value\" ($type) was not accepted by check$optname_die_str";
    }
  } elsif (ref($self->{check}) =~ /(^|::)Regexp$/) {
    # check based on regexp

    my $re = $self->{check};
    if ($value =~ /$re/) {
      return $value;
    } else {
      croak "Value \"$value\" ($type) was not accepted by regexp check$optname_die_str";
    }
  } elsif (ref $self->{check} eq 'ARRAY') {
    if (!defined $value && grep { !defined } @{ $self->{check} }) {
      return $value;
    }
    if (defined $value && grep { $_ eq $value } @{ $self->{check} }) {
      return $value;
    }
    $value = defined $value ? "\"$value\"" : "<undef>";
    croak "Invalid value $value for enum$optname_die_str";
  }

  die "Unknown check type: @{[ ref $self->{check} ]}$optname_die_str"
}

sub type {
  my ($self) = @_;

  return $self->{type} || "string";
}

sub storage {
  my ($self) = @_;

  # should be empty string (defined) on scalar, "HASH" on hash, "ARRAY" on
  # array (because of construction; see App::Getconf->opt())
  return $self->{storage};
}

sub help {
  my ($self) = @_;

  return $self->{help};
}

sub has_value {
  my ($self) = @_;

  return exists $self->{value};
}

sub value {
  my ($self) = @_;

  return $self->{value};
}

sub default {
  my ($self) = @_;

  return $self->{default};
}

sub has_default {
  my ($self) = @_;

  return exists $self->{default};
}

sub alias {
  my ($self) = @_;

  return $self->{alias};
}

#-----------------------------------------------------------------------------
1;
# vim:ft=perl
