fold: /TIMES|PLUS|DIV|MINUS/(NUM($n), $op, NUM($m)) 
  => { 
    $op = $op->{attr};
    $n->{attr} = eval  "$n->{attr} $op $m->{attr}";
    $_[0] = $NUM[0]; # return true value
  }
zero_times_whatever: TIMES(NUM($x), ., .) and { $x->{attr} == 0 } => { $_[0] = $NUM }
whatever_times_zero: TIMES(., ., NUM($x)) and { $x->{attr} == 0 } => { $_[0] = $NUM }

/* rules related with times */
times_zero = zero_times_whatever whatever_times_zero;

