package T;

# This module has been generated using Parse::Eyapp::Treereg
# from file Transform4.trg. Don't modify it.
# Change Transform4.trg instead.
# Copyright (c) Casiano Rodriguez-Leon 2006. Universidad de La Laguna.
# You may use it and distribute it under the terms of either
# the GNU General Public License or the Artistic License,
# as specified in the Perl README file.

use strict;
use warnings;
use Carp;
use Parse::Eyapp::_TreeregexpSupport qw(until_first_match checknumchildren);

#line 11 "Transform4.trg"
our @times_zero = Parse::Eyapp::YATW->buildpatterns(zero_times_whatever => \&zero_times_whatever, whatever_times_zero => \&whatever_times_zero,);
our @all = ( our $fold, our $zero_times_whatever, our $whatever_times_zero, ) = Parse::Eyapp::YATW->buildpatterns(fold => \&fold, zero_times_whatever => \&zero_times_whatever, whatever_times_zero => \&whatever_times_zero, );


  sub fold { 
    my $fold = $_[3]; # reference to the YATW pattern object
    my @NUM;
    my $n;
    my $W;
    my $m;
    my $op;

    {
      my $child_index = 0;

      return 0 unless ref($W = $_[$child_index]) =~ m{\bTIMES\b|\bPLUS\b|\bDIV\b|\bMINUS\b}x;
    return 0 unless (ref($NUM[0] = $W->child(0+$child_index)) eq 'R::NUM');
    return 0 unless defined($n = $NUM[0]->child(0+$child_index));
    return 0 unless defined($op = $W->child(1+$child_index));
    return 0 unless (ref($NUM[1] = $W->child(2+$child_index)) eq 'R::NUM');
    return 0 unless defined($m = $NUM[1]->child(0+$child_index));

    } # end block of child_index
#line 2 "Transform4.trg"
  {  
    $op = $op->{attr};
    $n->{attr} = eval  "$n->{attr} $op $m->{attr}";
    $_[0] = $NUM[0]; # return true value
  }

  } # end of fold 


  sub zero_times_whatever { 
    my $zero_times_whatever = $_[3]; # reference to the YATW pattern object
    my $NUM;
    my @W;
    my $TIMES;
    my $x;

    {
      my $child_index = 0;

      return 0 unless (ref($TIMES = $_[$child_index]) eq 'R::TIMES');
    return 0 unless (ref($NUM = $TIMES->child(0+$child_index)) eq 'R::NUM');
    return 0 unless defined($x = $NUM->child(0+$child_index));
    return 0 unless defined($W[0] = $TIMES->child(1+$child_index));
    return 0 unless defined($W[1] = $TIMES->child(2+$child_index));
    return 0 unless do 
#line 7 "Transform4.trg" 
      { $x->{attr} == 0 };

    } # end block of child_index
#line 7 "Transform4.trg"
  {  $_[0] = $NUM }

  } # end of zero_times_whatever 


  sub whatever_times_zero { 
    my $whatever_times_zero = $_[3]; # reference to the YATW pattern object
    my $NUM;
    my @W;
    my $TIMES;
    my $x;

    {
      my $child_index = 0;

      return 0 unless (ref($TIMES = $_[$child_index]) eq 'R::TIMES');
    return 0 unless defined($W[0] = $TIMES->child(0+$child_index));
    return 0 unless defined($W[1] = $TIMES->child(1+$child_index));
    return 0 unless (ref($NUM = $TIMES->child(2+$child_index)) eq 'R::NUM');
    return 0 unless defined($x = $NUM->child(0+$child_index));
    return 0 unless do 
#line 8 "Transform4.trg" 
      { $x->{attr} == 0 };

    } # end block of child_index
#line 8 "Transform4.trg"
  {  $_[0] = $NUM }

  } # end of whatever_times_zero 

#line 13 "Transform4.trg"

=head1 SYNOPSIS

Compile it with 

         eyapp -m 'Calc' Rule9.yp 
         treereg -o T.pm -p 'R::' -m T Transform4

to execute, run C<foldand0rule9_4.pl> 

=cut


1;

