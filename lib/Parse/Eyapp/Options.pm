#
# Module Parse::Eyapp::Options
#
# This module is based on Francois Desarmenien Parse::Yapp module
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien, all rights reserved.
# (c) Parse::Eyapp Copyright 2006 Casiano Rodriguez-Leon, all rights reserved.

package Parse::Eyapp::Options;

use strict;
use Carp;

############################################################################
#Definitions of options
#
# %known_options    allowed options
#
# %default_options  default
#
# %actions          sub refs to execute if option is set with ($self,$value)
#                   as parameters
############################################################################
#
#A value of '' means any value can do
#
my(%known_options)= (
    language    =>  {
        perl    => "Ouput parser for Perl language",
# for future use...
#       'c++'   =>  "Output parser for C++ language",
#       c       =>  "Output parser for C language"
    },
    linenumbers =>  {
        0       =>  "Don't embbed line numbers in parser",
        1       =>  "Embbed source line numbers in parser"
    },
    firstline   =>  {
        ''      =>  "Line number where the input grammar starts"
    },
    inputfile   =>  {
        ''      =>  "Input file name: will automagically fills input"
    },
    classname   =>  {
        ''      =>  "Class name of parser object (Perl and C++)"
    },
    standalone  =>  {
        0       =>  "Don't create a standalone parser (Perl and C++)",
        1       =>  "Create a standalone parser"
    },
    input       =>  {
        ''      =>  "Input text of grammar"
    },
    template    => {
        ''      =>  "Template text for generating grammar file"
    },
);

my(%default_options)= (
    language => 'perl',
    firstline => 1,
    linenumbers => 1,
    inputfile => undef,
    classname   => 'Parser',
    standalone => 0,
    input => undef,
    template => undef,
    shebang => undef,
);

my(%actions)= (
    inputfile => \&__LoadFile
);

#############################################################################
#
# Actions
#
# These are NOT a method, although they look like...
#
# They are super-private routines (that's why I prepend __ to their names)
#
#############################################################################
sub __LoadFile {
    my($self,$filename)=@_;

    return if defined($self->{OPTIONS}{input});

        open(IN,"<$filename")
    or  croak "Cannot open input file '$filename' for reading";
    $self->{OPTIONS}{input}=join('',<IN>);
    close(IN);
}

#############################################################################
#
# Private methods
#
#############################################################################

sub _SetOption {
    my($self)=shift;
    my($key,$value)=@_;

    $key=lc($key);

        @_ == 2
    or  croak "Invalid number of arguments";

        exists($known_options{$key})
    or  croak "Unknown option: '$key'";

    if(exists($known_options{$key}{lc($value)})) {
        $value=lc($value);
    }
    elsif(not exists($known_options{$key}{''})) {
        croak "Invalid value '$value' for option '$key'";
    }

        exists($actions{$key})
    and &{$actions{$key}}($self,$value);

    $self->{OPTIONS}{$key}=$value;
}

sub _GetOption {
    my($self)=shift;
    my($key)=map { lc($_) } @_;

        @_ == 1
    or  croak "Invalid number of arguments";

        exists($known_options{$key})
    or  croak "Unknown option: '$key'";

    $self->{OPTIONS}{$key};
}

#############################################################################
#
# Public methods
#
#############################################################################

#
# Constructor
#
sub new {
    my($class)=shift;
    my($self)={ OPTIONS => { %default_options } };

        ref($class)
    and $class=ref($class);
    
    bless($self,$class);

    $self->Options(@_);

    $self;
}

#
# Specify one or more options to set
#
sub Options {
    my($self)=shift;
    my($key,$value);

        @_ % 2 == 0
    or  croak "Invalid number of arguments";

    while(($key,$value)=splice(@_,0,2)) {
        $self->_SetOption($key,$value);
    }
}

#
# Set (2 parameters) or Get (1 parameter) values for one option
#
sub Option {
    my($self)=shift;
    my($key,$value)=@_;

        @_ == 1
    and return $self->_GetOption($key);

        @_ == 2
    and return $self->_SetOption($key,$value);

    croak "Invalid number of arguments";

}

1;

__END__

=head1 Parse::Eyapp::Options
 
Parse::Eyapp::Options - Implements the analysis of options when parsing Eyapp grammars
 
=head1 SEE ALSO
  
No documentation here. To learn about Parse::Eyapp::Options see the following
documents:
  
=over

=item * L<Parse::Eyapp>,

=item * L<http://nereida.deioc.ull.es/~pl/perlexamples/section_eyappts.html> (Spanish),

=item * L<eyapp>,

=item * L<treereg>,

=item * L<Parse::yapp>,

=item * yacc(1),

=item * bison(1),

=item * The classic book "Compilers: Principles, Techniques, and Tools" by Alfred V. Aho, Ravi Sethi and

=item * Jeffrey D. Ullman (Addison-Wesley 1986)

=item * L<Parse::RecDescent>.

=back

=head1 AUTHOR
 
Casiano Rodriguez-Leon (casiano@ull.es)
 
=head1 ACKNOWLEDGMENTS

This work has been supported by CEE (FEDER) and the Spanish Ministry of
Educación y Ciencia through Plan Nacional I+D+I number TIN2005-08818-C04-04
(ULL::OPLINK project). Support from Gobierno de Canarias was through GC02210601
(Grupos Consolidados).
The University of La Laguna has also supported my work in many ways
and for many years.
I wish to thank Francois Desarmenien for his C<Parse::Yapp> module,
to my students at La Laguna and to the Perl Community. Special thanks to
my family and Larry Wall.

=head1 LICENCE AND COPYRIGHT

Copyright (c) 2006 Casiano Rodriguez-Leon (casiano@ull.es). All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


