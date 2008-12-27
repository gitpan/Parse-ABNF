package Parse::ABNF;

use 5.006;
use strict;
use warnings;
use Parse::RecDescent;

our $VERSION = '0.01';
our $Grammar = q{

  {
    sub Make {
      my $class = shift;
      my %opts = @_;

      # TODO: this could need some revision

      return $opts{value}->[0] if $class eq 'Group'
        and ref $opts{value} eq 'ARRAY' and @{$opts{value}} == 1;

      return $opts{value} if $class eq 'Group'
        and ref $opts{value} ne 'ARRAY';

      return $opts{value}->[0] if $class eq 'Choice'
        and ref $opts{value} eq 'ARRAY' and @{$opts{value}} == 1;

      return $opts{value}->[0] if $class eq 'Choice'
        and ref $opts{value} ne 'ARRAY';

      return { class => $class, %opts };
    }
  }

  parse: rulelist {
    $return = $item[1];
  }

  # 
  rulelist: rule(s) {
    # TODO: this is not a nice way to get rid of empty lines
    $return = [grep ref, @{$item[1]}];
  }

  rule: c_wsp(s?) c_nl 

  rule: rulename c_wsp(s?) "=" c_wsp(s?) elements c_nl {
    $return = Make(Rule => name => $item[1], value => $item[5]);
  }

  rule: rulename c_wsp(s?) "=/" c_wsp(s?) elements c_nl {
    $return = Make(Rule => name => $item[1], value => $item[5], combine => 'choice');
    
  }

  rulename: /[a-zA-Z0-9-]+/ {
    $return = $item[1];
  }

  # n exactly
  repetition: /\d+/ element {
    $return = Make(Repetition => min => $item[1], max => $item[1], value => $item[2]);
  }

  # n to m
  repetition: /\d+/ "*" /\d+/ element {
    $return = Make(Repetition => min => $item[1], max => $item[3], value => $item[4]);
  }

  # 0 to n
  repetition: "*" /\d+/ element {
    $return = Make(Repetition => min => 0, max => $item[2], value => $item[3]);
  }

  # n or more
  repetition: /\d+/ "*" element {
    $return = Make(Repetition => min => $item[1], max => undef, value => $item[3]);
  }

  # zero or more
  repetition: "*" element {
    $return = Make(Repetition => min => 0, max => undef, value => $item[2]);
  }

  # exactly one
  repetition: element {
     $return = $item[1];
  }

  # 
  elements: alternation c_wsp(s?) {
    $return = $item[1];
  }

  #
  alternation: concatenation (c_wsp(s?) "/" c_wsp(s?) concatenation)(s?) {
    $return = Make(Choice => value => [$item[1], @{$item[2]}]);
  }

  #
  concatenation: repetition (c_wsp(s) repetition)(s?) {
    $return = Make(Group => value => [$item[1], @{$item[2]}]);
  }

  #
  element: ref_val | group | option | char_val | num_val | prose_val {
    $return = $item[1];
  }

  ref_val: rulename {
    $return = Make(Reference => name => $item[1]);
  }

  # 
  group: "(" c_wsp(s?) alternation c_wsp(s?) ")" {
    $return = Make(Group => value => $item[3]);
  }

  #
  option: "[" c_wsp(s?) alternation c_wsp(s?) "]" {
    my $group = Make(Group => value => $item[3]);
    $return = Make(Repetition => min => 0, max => 1, value => $group);
  }

  c_wsp: /[ \t]/

  c_wsp: c_nl /[ \t]/

  c_nl: "\n"

  c_nl: comment

  comment: /;[ \t\x21-\x7e]*\n/

  char_val: '"' /[\x20-\x21\x23-\x7E]*/ '"' {
    $return = Make(Literal => value => $item[2]);
  }

  num_val: bin_val | dec_val | hex_val {
    $return = $item[1];
  }

  bin_val: "%b" /[01]+/ "-" /[01]+/ {
    $return = Make(Range => type => 'binary', min => $item[2], max => $item[4]);
  }

  dec_val: "%d" /\d+/ "-" /\d+/ {
    $return = Make(Range => type => 'decimal', min => $item[2], max => $item[4]);
  }

  hex_val: "%x" /[0-9a-fA-F]+/ "-" /[0-9a-fA-F]+/ {
    $return = Make(Range => type => 'hex', min => $item[2], max => $item[4]);
  }

  # TODO: I would like to combine the one/many cases but
  # the first attempt failed for some unknown reason.

  bin_val: "%b" /[01]+/ /(?:\.[01]+)+/  {
    my @trails = split /\./, $item[3];
    $return = Make(String => type => 'binary', value => [@trails[1 .. $#trails]]);
  }

  bin_val: "%b" /[01]+/ {
    $return = Make(String => type => 'binary', value => [$item[2]]);
  }

  dec_val: "%d" /\d+/ /(?:\.\d+)+/  {
    my @trails = split /\./, $item[3];
    $return = Make(String => type => 'decimal', value => [@trails[1 .. $#trails]]);
  }

  dec_val: "%d" /\d+/ {
    $return = Make(String => type => 'decimal', value => [$item[2]]);
  }

  hex_val: "%x" /[0-9a-fA-F]+/ /(?:\.[0-9a-fA-F]+)+/  {
    my @trails = split /\./, $item[3];
    $return = Make(String => type => 'hex', value => [@trails[1 .. $#trails]]);
  }

  hex_val: "%x" /[0-9a-fA-F]+/ {
    $return = Make(String => type => 'hex', value => [$item[2]]);
  }

  prose_val: "<" /[\x20-\x3d\x3f-\x7e]*/ ">" {
    $return = Make(ProseValue => value => $item[2]);
  }

};

my $CoreRulesGrammar = q{

ALPHA          =  %x41-5A / %x61-7A
BIT            =  "0" / "1"
CHAR           =  %x01-7F
CR             =  %x0D
CRLF           =  CR LF
CTL            =  %x00-1F / %x7F
DIGIT          =  %x30-39
DQUOTE         =  %x22
HEXDIG         =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
HTAB           =  %x09
LF             =  %x0A
LWSP           =  *(WSP / CRLF WSP)
OCTET          =  %x00-FF
SP             =  %x20
VCHAR          =  %x21-7E
WSP            =  SP / HTAB

};

our $CoreRules = do {
  __PACKAGE__->new->parse( $CoreRulesGrammar );
};

sub new {
  my $class = shift;
  local $Parse::RecDescent::skip = '';
  bless { _p => Parse::RecDescent->new($Grammar) }, $class;
}

sub parse {
  my $self = shift;
  my $string = shift;

  return $self->{_p}->parse($string)
}

1;

__END__

=head1 NAME

Parse::ABNF - Parse IETF Augmented BNF (ABNF) grammars.

=head1 SYNOPSIS

  use Parse::ABNF;
  my $parser = Parse::ABNF->new;
  my $rules = $parser->parse($grammar);

=head1 DESCRIPTION

This module parses IETF ABNF (STD 68, RFC 5234, 4234, 2234) grammars into
a list of rules. Artifacts are mapped into hash references as follows:

  A  = B ~ { class => 'Rule',       value => B, name => A               }
  A /= B ~ { class => 'Rule',       value => B, ... combine => 'choice' }
  A / B  ~ { class => 'Choice',     value => [A, B]                     }
  A B    ~ { class => 'Group',      value => [A, B]                     }
  A      ~ { class => 'Reference',  name  => A                          }
  n*mA   ~ { class => 'Repetition', value => A, min  => n, max => m     }
  [ A ]  ~ { class => 'Repetition', value => A, min  => 0, max => 1     }
  *A     ~ { class => 'Repetition', value => A, min  => 0, max => undef }
  "A"    ~ { class => 'Literal',    value => A                          }
  <A>    ~ { class => 'ProseValue', value => A                          }
  %xA.B  ~ { class => 'String',     value => [A, B], type => 'hex'      }
  %bA.B  ~ { class => 'String',     value => [A, B], type => 'binary'   }
  %dA.B  ~ { class => 'String',     value => [A, B], type => 'decimal'  }
  %xA-B  ~ { class => 'Range',      type  => 'hex', min => A, max => B  }

Forms not listed here are mapped in an analogous manner.

As an example, the ABNF grammar

  A = (B C) / *D

is parsed into

  [ {
    'value' => {
      'value' => [
        {
          'value' => [
            {
              'name' => 'B',
              'class' => 'Reference'
            },
            {
              'name' => 'C',
              'class' => 'Reference'
            }
          ],
          'class' => 'Group'
        },
        {
          'min' => 0,
          'value' => {
            'name' => 'D',
            'class' => 'Reference'
          },
          'max' => undef,
          'class' => 'Repetition'
        }
      ],
      'class' => 'Choice'
    },
    'name' => 'A',
    'class' => 'Rule'
  } ]

Until this module matures, this format is subject to change. Contact the
author if you would like to depend on this module.

=head1 CORE RULES

The ABNF specification defines some Core Rules that are used without
defining them locally in many ABNF grammars. You can access these rules
as parsed by this module via C<$Parser::ABNF::CoreRules>.

=head1 CAVEATS

Instead of CRLF line endings this module expects "\n" as line terminator.
If necessary, convert the line endings e.g. using

  $grammar =~ s/\x0d\x0a/\n/g;

The ABNF specification disallows white space preceding the left hand side,
and so does this module. Remove it prior to passing the grammar e.g. using

  $grammar =~ s/^\s+(?=\w+\s*=)//mg;

This module does not do that for you in order to preserve line and column
numbers. Patches adapting the grammar to allow leading white space welcome.

The ABNF specification allows non-terminals to be enclosed inside <...>.
That is the same syntax as used for prose values, and this module makes no
attempt to differentiate the two.

Comments are not currently made available, this may change in future versions.

There is currently not much error handling. Patches welcome.

=head1 BUG REPORTS

Please report bugs in this module via
L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Parser-ABNF>

=head1 SEE ALSO

  * http://www.ietf.org/rfc/rfc5234.txt
  * Parse::RecDescent

=head1 AUTHOR / COPYRIGHT / LICENSE

  Copyright (c) 2008-2009 Bjoern Hoehrmann <bjoern@hoehrmann.de>.
  This module is licensed under the same terms as Perl itself.

=cut
