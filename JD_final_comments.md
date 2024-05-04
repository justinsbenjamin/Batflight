It would be good to talk more about feeding protocol, especially for control group (you say you expect them to lose weight).

F1 should be more clear (including axis labels) that you are interested in the _relative_ loss of mass of exercised vs. controls.

Weight loss explanation is not clear: First of all, I assume you mean -3g/60g. Second, is that per day? That would be a unit. Or is it over the course of the experiment? That should be stated explicitly. Similarly, random effects that have units should be listed with units.

F3 has similar clarity problems to F1. It also highlights a “tails” problem. Is there really _no_ backwards effect that you would be interested in at all. Like, if fasting and trialling without exercise worked much _better_ than with exercise, would you really be like: we don't care? Because that's kind of what your big “not meaningful” areas are saying.

“We expect the proportion of CIs” is not strong enough. Nominal coverage is the standard for whether the statistical test is working, although grey areas like this (test seems reasonable, coverage not _terrible_) are hard to navigate.

Be careful in interpretion “proportion of CIs whose upper bounds were lower than 0 was one”. You already know that the absolutely exactly known true value is negative. So why _did_ you calculate that proportion? This is a pedagogical question, there is a good answer.

It would have been cool to test the simulations _with_ REs against the model without.

The confusion in shake land seems to be about the link function. The “2” that you put in was in the link scale, so it corresponded to the natural log of the mean that you were simulating.

Also, the paper has no Discussion (everything in the Discussion seems to obviously belong in Results).
