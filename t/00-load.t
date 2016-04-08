use Test::More;

BEGIN { use_ok 'QMail::QueueHandler' };

my $qh;
eval { ok($qh = QMail::QueueHandler->new) };

isa_ok($qh, 'QMail::QueueHandler');

done_testing();
