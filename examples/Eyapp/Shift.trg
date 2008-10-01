# File: Shift.trg
{
  sub log2 { 
    my $n = shift; 
    return log($n)/log(2); 
  }

  my $power;
}
mult2shift: TIMES($e, NUM($m)) 
  and { $power = log2($m->{attr}); (1 << $power) == $m->{attr} } => { 
    $_[0]->delete(1);
    $_[0]->{shift} = $power;
    $_[0]->type('SHIFTLEFT');
  }
