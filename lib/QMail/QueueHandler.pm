package QMail::QueueHandler;

# QMail::QueueHander
#
# Copyright (c) 2016 Dave Cross <dave@perlhacks.com>
# Based on original version by Michele Beltrame <mb@italpro.net>
#
# This program is distributed under the GNU GPL.
# For more information have a look at http://www.gnu.org

use Moose;

use Term::ANSIColor;
use Getopt::Std;
use File::Basename;

my $version = '2.0.0 [alpha]';
my $me = basename $0;

# Where qmail stores all of its files
has queue => (
  is => 'ro',
  isa => 'Str',
  default => '/var/qmail/queue/',
);

# Which todo format do we have?
has bigtodo => (
  is => 'ro',
  isa => 'Bool',
  lazy => 1,
  default => sub { -d $_[0]->queue . 'todo/0' },
);

# Various commands that we use
has commands => (
  is => 'ro',
  isa => 'HashRef',
  default => sub { {
    start => 'service qmail start',
    stop  => 'service qmail stop',
    pid   => 'pidof qmail-send',
  } },
);

# Colours for output.
# Default is non-coloured. These values can ve changed in parse_args.
has colours => (
  is => 'ro',
  isa => 'HashRef',
  default => sub { {
    msg => '',
    stat => '',
    end => '',
  } },
);

# Are we showing a summary?
has summary => (
  is => 'ro',
  isa => 'Bool',
  default => 0,
);

# Are we supposed to be deleting things?
has deletions => (
  is => 'rw',
  isa => 'Bool',
);

# What actions are we carrying out.
# Each element in this array is another array.
# The first element in these second level arrays is a code ref.
# The other elements are arguments to be passed to the code ref.
has actions => (
  is => 'ro',
  traits => ['Array'],
  isa => 'ArrayRef',
  default => sub { [] },
  handles => {
      add_action => 'push',
      all_actions => 'elements',
  },
);

# Do we need to restart QMail once we have finished?
has restart => (
  is => 'ro',
  isa => 'Bool',
  default => 0,
);

# List of messages to delete
has to_delete => (
  is => 'rw',
  isa => 'ArrayRef',
  default => sub { [] },
);

# List of messages to flag
has to_flag => (
  is => 'rw',
  isa => 'ArrayRef',
  default => sub { [] },
);

# Hash containing details of the messages in the queue
has msglist => (
  is => 'rw',
  isa => 'HashRef',
  lazy_build => 1,
);

####################  USER CONFIGURATION END  ####################

sub BUILD {
    my $self = shift;

    # Get command line options
    $self->parse_args;
}

sub run {
    my $self = shift;
    my @args = @_;

    # If we want to delete stuff, then stop qmail.
    if ($self->deletions) {
        $self->stop_qmail;
    }

    # Execute actions
    foreach my $action ($self->all_actions) {
        my $sub = shift @$action; # First element is the sub
        $self->$sub(@$action);  # Others the arguments, if any
    }

    # If we have planned deletions, then do them.
    if (@{ $self->to_delete }) {
        $self->trash_msgs();
    }

    # If we stopped qmail, then restart it
    $self->start_qmail();
}

# ##### SERVICE FUNCTIONS #####

sub _build_msglist {
    my $self = shift;
    
    my $queue = $self->queue;

    my (%todohash, %bouncehash);
    my $msglist = {};

    opendir(my $tododir,"${queue}todo");
    if ($self->bigtodo) {
        foreach my $todofile (grep { !/\./ } readdir $tododir) {
            $todohash{$todofile} = $todofile;
        }
    } else {
        foreach my $tododir (grep { !/\./ } readdir $tododir) {
            opendir (my $subdir,"${queue}todo/$tododir");
            foreach my $todofile (grep { !/\./ }
                                  map  { "$tododir/$_" } readdir $subdir) {
                $msglist->{ $todofile }{ 'todo' } = $todofile;
            }
        }
    }
    closedir $tododir;

    opendir(my $bouncedir,"${queue}bounce");
    foreach my $bouncefile (grep { !/\./ } readdir $bouncedir) {
        $bouncehash{$bouncefile} = 'B';
    }
    closedir $bouncedir;


    opendir(my $messdir,"${queue}mess");
    foreach my $dir (grep { !/\./ } readdir $messdir) {

        opendir (my $infosubdir,"${queue}info/$dir");

        foreach my $infofile (grep { !/\./ }
                              map  { "$dir/$_" } readdir $infosubdir) {
            $msglist->{$infofile}{sender} = 'S';
        }

        close $infosubdir;

        opendir (my $localsubdir,"${queue}local/$dir");

        foreach my $localfile (grep { !/\./ }
                               map  { "$dir/$_" } readdir $localsubdir) {
            $msglist->{$localfile}{local} = 'L';
        }

        close $localsubdir;

        opendir (my $remotesubdir,"${queue}remote/$dir");

        foreach my $remotefile (grep { !/\./ }
                                map  { "$dir/$_" } readdir $remotesubdir) {
            $msglist->{$remotefile}{remote} = 'R';
        }

        close $remotesubdir;

        opendir (my $subdir,"${queue}mess/$dir");

        foreach my $file (grep { !/\./ }
                          map  { "$dir/$_" } readdir $subdir) {
            my ($dirno, $msgno) = split(/\//, $file);
            if ($bouncehash{$msgno}) {
                $msglist->{ $file }{bounce} = 'B';
            }
            if ($self->bigtodo) {
                if ($todohash{$msgno}) {
                    $msglist->{ $file }{todo} = $msgno;
                }
            }
        }

        closedir $subdir;
    }
    closedir $messdir;

    return $msglist;
}

sub parse_args {
    my $self = shift;

    @ARGV or usage();

    my %opt;

    my %optargs = (
        a => 0,
        l => 0,
        L => 0,
        R => 0,
        N => 0,
        c => 0,
        s => 0,
        m => 1,
        f => 1,
        F => 1,
        d => 1,
        S => 1,
        h => 1,
        b => 1,
        H => 1,
        B => 1,
        t => 1,
        D => 0,
        V => 0,
    );

    my $optstring = join '',
                    map { $_ . ($optargs{$_} ? ':' : '') }
                    keys %optargs;

    getopts($optstring, \%opt);

    foreach my $opt (keys %opt) {
        if ($optargs{$opt} and not $opt{$opt}) {
            die "Option $opt must have an argument\n";
        }
        SWITCH: {
            $opt eq 'a' and do {
                $self->add_action([\&send_msgs]);
                last SWITCH;
            };
            $opt eq 'l' and do {
                $self->add_action([\&list_msg, 'A']);
                last SWITCH;
            };
            $opt eq 'L' and do {
                $self->add_action([\&list_msg, 'L']);
                last SWITCH;
            };
            $opt eq 'R' and do {
                $self->add_action([\&list_msg, 'R']);
                last SWITCH;
            };
            $opt eq 'N' and do {
                $self->summary(1);
                last SWITCH;
            };
            $opt eq 'c' and do {
                @{ $self->colours }{qw[msg stat end]} = (
                    color('bold bright_blue'),
                    color('bold bright_red'),
                    color('reset'),
                );
                last SWITCH;
            };
            $opt eq 's' and do {
                $self->add_action([\&stats]);
                last SWITCH;
            };
            $opt eq 'm' and do {
                $self->add_action([\&view_msg, $opt{$opt}]);
                last SWITCH;
            };
            $opt eq 'f' and do {
                $self->add_action([\&del_msg_from_sender, $opt{$opt}]);
                $self->deletions(1);
                last SWITCH;
            };
            $opt eq 'F' and do {
                $self->add_action([\&del_msg_from_sender_r, $opt{$opt}]);
                $self->deletions(1);
                last SWITCH;
            };
            $opt eq 'd' and do {
                $self->add_action([\&del_msg, $opt{$opt}]);
                $self->deletions(1);
                last SWITCH;
            };
            $opt eq 'S' and do {
                $self->add_action([\&del_msg_subj, $opt{$opt}]);
                $self->deletions(1);
                last SWITCH;
            };
            $opt eq 'h' and do {
                $self->add_action([\&del_msg_header_r, 'I', $opt{$opt}]);
                $self->deletions(1);
                last SWITCH;
            };
            $opt eq 'b' and do {
                $self->add_action([\&del_msg_body_r, 'I', $opt{$opt}]);
                $self->deletions(1);
                last SWITCH;
            };
            $opt eq 'H' and do {
                $self->add_action([\&del_msg_header_r, 'C', $opt{$opt}]);
                $self->deletions(1);
                last SWITCH;
                };
            $opt eq 'B' and do {
                $self->add_action([\&del_msg_body_r, 'C', $opt{$opt}]);
                $self->deletions(1);
                last SWITCH;
            };
            $opt eq 't' and do {
                $self->add_actions([\&flag_remote, $opt{$opt}]);
                last SWITCH;
            };
            $opt eq '-D' and do {
                $self->add_action([\&del_all]);
                $self->deletions(1);
                last SWITCH;
            };
            $opt eq '-V' and do {
                $self->add_action([\&version]);
                last SWITCH;
                };
            $self->usage;
        }
    }

    return;
}

# Stop qmail
sub stop_qmail {
    my $self = shift;

    # If qmail is running, we stop it
    if (my $qmpid = $self->qmail_pid) {

        # If there is a system script available, we use it
        if ($self->commands->{stopqmail} ne '') {

            warn "Calling system script to terminate qmail...\n";
            if (system($self->commands->{stopqmail}) > 0) {
                die 'Could not stop qmail';
            }
            sleep 1 while $self->qmail_pid;

        # Otherwise, we're killers!
        } else {
            warn "Terminating qmail (pid $qmpid)... ",
                 "this might take a while if qmail is working.\n";
            kill 'TERM', $qmpid;
    
            sleep 1 while $self->qmail_pid;
        }

    # If it isn't, we don't. We also return a false value so our caller
    # knows they might not want to restart it later.
    } else {
        warn "Qmail isn't running... no need to stop it.\n";
        return;
    }

    $self->restart(1);

    return 1;
}

# Start qmail
sub start_qmail {
    my $self = shift;

    return unless $self->restart;

    # If qmail is running, why restart it?
    if (my $qmpid = $self->qmail_pid) {
        warn "Qmail is already running again, so it won't be restarted.\n";
        return 1;
    }

    # In any other case, we restart it
    warn "Restarting qmail... \n";
    system($self->commands->{startqmail});
    warn "Done (hopefully).\n";

    return 1;
}

# Returns the subject of a message
sub get_subject {
    my $self = shift;
    my ($msg) = @_;

    my $msgsub;
    my $queue = $self->queue;
    open (my $msg_fh, '<', "${queue}mess/$msg")
        or die("cannot open message $msg! Is qmail-send running?\n");
    while (<$msg_fh>) {
        if ( /^Subject: (.*)/) {
            $msgsub = $1;
            chomp $msgsub;
        } elsif ( $_ eq "\n") {
            last;
        }
    }
    close ($msg_fh);
    return $msgsub;
}

sub get_sender {
    my $self = shift;
    my ($msg) = @_;

    my $queue = $self->queue;

    open (my $msg_fh, '<', "${queue}/info/$msg")
        or die("cannot open info file ${queue}/info/$msg! ",
               "Is qmail-send running?\n");
    my $sender = <$msg_fh>;
    substr($sender, 0, 1) = '';
    chomp $sender;
    close ($msg_fh);
    return $sender;
}


# ##### MAIN FUNCTIONS #####

# Tries to send all queued messages now 
# This is achieved by sending an ALRM signal to qmail-send
sub send_msgs {
    my $self = shift;

    # If qmail is running, we force sending of messages
    if (my $qmpid = $self->qmail_pid) {

        kill 'ALRM', $qmpid;

    } else {

        warn "Qmail isn't running, can't send messages!\n";

    }
    return;
}

sub show_msg_info {
    my $self = shift;
    my ($msg_id) = @_;

    my %msg;
    my $queue = $self->queue;

    open (my $info_fh, '<', "${queue}info/$msg_id");
    $msg{ret} = <$info_fh>;
    substr($msg{ret}, 0, 1) = '';
    chomp $msg{ret};
    close ($info_fh);
    my ($dirno, $rmsg) = split(/\//, $msg_id);
    print "$rmsg ($dirno, $msg_id)\n";
 
    # Get message (file) size
    $msg{fsize} = (stat("${queue}mess/$msg_id"))[7];

    my %header = (
        Date    => 'date',
        From    => 'from',
        Subject => 'subject',
        To      => 'to',
        Cc      => 'cc',
    );

    # Read something from message header (sender, receiver, subject, date)
    open (my $msg_fh, '<', "${queue}mess/$msg_id");
    while (<$msg_fh>) {
        chomp;
        foreach my $h (keys %header) {
            if (/^$h: (.*)/) {
                $msg{$header{$h}} = $1;
                last;
            }
        }
    }
    close($msg_fh);

    # Add "pseudo-headers" for output
    $header{'Return-path'} = 'ret';
    $header{Size}          = 'fsize';

    my $colours = $self->colours;
    my ($cmsg, $cend) = @{$colours}{qw[msg end]};

    for (qw[Return-path From To Cc Subject Date Size]) {
        next unless exists $msg{$header{$_}};

        print "  ${cmsg}$_${cend}: $msg{$header{$_}}\n";
    }

    return;
}

# Display message list
# pass parameter of queue NOT to list! i.e. if you want remote only, pass L
# if you want local, pass R  if you want all pass anything else eg A
sub list_msg {
    my $self = shift;
    my ($q) = @_;

    my $msglist = $self->msglist;
    for my $msg (keys %$msglist) {
        if (!$self->summary) {
            if ($q eq 'L') {
                if ($msglist->{$msg}{local}) {
                    $self->show_msg_info($msg);
                }
            }
            if ($q eq 'R') {
                if ($msglist->{$msg}{remote}) {
                    $self->show_msg_info($msg);
                }
            }
            if ($q eq 'A') {
                if ($msglist->{$msg}{local}) {
                    $self->show_msg_info($msg);
                }
                if ($msglist->{$msg}{remote}) {
                    $self->show_msg_info($msg);
                }
            }
        } ## end if ($summary == 0)
    } ## end foreach my $msg (@msglist)

    $self->stats();
    return;
}

# View a message in the queue
#
sub view_msg {
    my $self = shift;
    my ($rmsg) = @_;
    
    if ($rmsg =~ /\D/) {
        warn "$rmsg is not a valid message number!\n";
        return;
    }

    # Search message
    my $ok = 0;
    my $queue = $self->queue;
    for my $msg(keys %{$self->msglist}) {
        if ($msg =~ /\/$rmsg$/) {
            $ok = 1;
            print "\n --------------\nMESSAGE NUMBER $rmsg \n --------------\n"; 
            open (my $msg_fh, '<', "${queue}mess/$msg");
            while (<$msg_fh>) {
                print $_;
            }
            close ($msg_fh);
            last;
        }
    }

    # If the message isn't found, print a notice
    if (!$ok) {
        warn "Message $rmsg not found in the queue!\n";    
    }

    return;    
}

sub trash_msgs {
    my $self = shift;

    my $queue = $self->queue;
    my $msglist = $self->msglist;
    my @todelete = ();
    my $grouped = 0;
    my $deleted = 0;
    foreach my $msg (@{$self->to_delete}) {
        $grouped++;
        $deleted++;
        my ($dirno, $msgno) = split(/\//, $msg);
        if ($msglist->{$msg}{bounce}) {
            push @todelete, "${queue}bounce/$msgno";
        }
        push @todelete, "${queue}mess/$msg";
        push @todelete, "${queue}info/$msg";
        if ($msglist->{$msg}{remote}) {
            push @todelete, "${queue}remote/$msg";
        }
        if ($msglist->{$msg}{local}) {
            push @todelete, "${queue}local/$msg";
        }
        if ($msglist->{$msg}{todo}) {
            push @todelete, "${queue}todo/$msglist->{$msg}{'todo'}";
            push @todelete, "${queue}intd/$msglist->{$msg}{'todo'}";
        }
        if ($grouped == 11) {
            unlink @todelete;
            @todelete = ();
            $grouped = 0;
        }
    }
    if ($grouped) {
        unlink @todelete;
    }
    warn "Deleted $deleted messages from queue\n";
    return;
}

sub flag_msgs {
    my $self = shift;

    my $queue = $self->queue;
    my $now = time;
    my @flagqueue = ();
    my $flagged = 0;
    foreach my $msg (@{$self->to_flag}) {
        push @flagqueue, "${queue}info/$msg";
        $flagged++;
        if ($flagged == 30) {
            utime $now, $now, @flagqueue;
            $flagged = 0;
            @flagqueue = ();
        }
    }
    if ($flagged) {
        utime $now, $now, @flagqueue;
    }
    return;
}

# Delete a message in the queue
sub del_msg {
    my $self = shift;
    my ($rmsg) = @_;
    
    if ($rmsg =~ /\D/) {
        warn "$rmsg is not a valid message number!\n";
        return;
    }

    # Search message
    my $ok = 0;
    for my $msg(keys %{$self->msglist}) {
        if ($msg =~ /\/$rmsg$/) {
            $ok = 1;
            push @{$self->to_delete}, $msg;
            warn "Deleting message $rmsg...\n";
            last;
        }
    }

    # If the message isn't found, print a notice
    if (!$ok) {
        warn "Message $rmsg not found in the queue!\n";
    }

    return;
}

sub del_msg_from_sender {
    my $self = shift;
    my ($badsender) = @_;

    warn "Looking for messages from $badsender\n";

    my $ok = 0;
    for my $msg (keys %{$self->msglist}) {
        if ($self->msglist->{$msg}{sender}) {
            my $sender = $self->get_sender($msg);
            if ($sender eq $badsender) {
                $ok = 1;
                my ($dirno, $msgno) = split(/\//, $msg);
                print "Message $msgno slotted for deletion\n";
                push @{$self->to_delete}, $msg;
            }
        }
    }
# If no messages are found, print a notice
    if (!$ok) {
        warn "No messages from $badsender found in the queue!\n";
    } 

    return;
}

sub del_msg_from_sender_r {
    my $self = shift;
    my ($badsender) = @_;

    warn "Looking for messages from senders matching $badsender\n";

    my $ok = 0;
    for my $msg (keys %{$self->msglist}) {
        if ($self->msglist->{$msg}{sender}) {
           my $sender = $self->get_sender($msg);
           if ($sender =~ /$badsender/) {
               $ok = 1;
               my ($dirno, $msgno) = split(/\//, $msg);
               print "Message $msgno slotted for deletion\n";
               push @{$self->to_delete}, $msg;
           }
        }
    }
# If no messages are found, print a notice
    if (!$ok) {
        warn "No messages from senders matching ",
             "$badsender found in the queue!\n";
    } 

    return;
}

sub del_msg_header_r {
    my $self = shift;
    my ($case, $re) = @_;

    warn "Looking for messages with headers matching $re\n";

    my $queue = $self->queue;
    my $ok = 0;
    for my $msg (keys %{$self->msglist}) {
    open (my $msg_fh, '<', "${queue}mess/$msg")
        or die("cannot open message $msg! Is qmail-send running?\n");
    while (<$msg_fh>) {
        if ($case eq 'C') {
            if (/$re/) {
                $ok = 1;
                my ($dirno, $msgno) = split(/\//, $msg);
                warn "Message $msgno slotted for deletion.\n";
                push @{$self->to_delete}, $msg;
                last;
            } elsif ( $_ eq "\n") {
                last;
            }
        } else {
            if (/$re/i) {
                $ok = 1;
                my ($dirno, $msgno) = split(/\//, $msg);
                warn "Message $msgno slotted for deletion.\n";
                push @{$self->to_delete}, $msg;
                last;
            } elsif ( $_ eq "\n") {
                last;
            }
        }
    }
    close ($msg_fh);

    }
    # If no messages are found, print a notice
    if (!$ok) {
        warn "No messages with headers matching $re found in the queue!\n";
    } 

    return;
}

sub del_msg_body_r {
    my $self = shift;
    my ($case, $re) = @_;

    my $nomoreheaders = 0;
    my $queue = $self->queue;

    warn "Looking for messages with body matching $re\n";

    my $ok = 0;
    for my $msg (keys %{$self->msglist}) {
        open (my $msg_fh, '<', "${queue}mess/$msg")
            or die("cannot open message $msg! Is qmail-send running?\n");
        while (<$msg_fh>) {
            if ($nomoreheaders == 1) {
                if ($case eq 'C') {
                    if (/$re/) {
                        $ok = 1;
                        my ($dirno, $msgno) = split(/\//, $msg);
                        warn "Message $msgno slotted for deletion.\n";
                        push @{$self->to_delete}, $msg;
                        last;
                    }
                } else {
                    if (/$re/i) {
                        $ok = 1;
                        my ($dirno, $msgno) = split(/\//, $msg);
                        warn "Message $msgno slotted for deletion.\n";
                        push @{$self->to_delete}, $msg;
                        last;
                    }
                }
            }
            else {
                if ($_ eq "\n") {
                    $nomoreheaders = 1;
                }
            }
        }
        close ($msg_fh);
        $nomoreheaders = 0;
    }
    # If no messages are found, print a notice
    if (!$ok) {
        warn "No messages with body matching $re found in the queue!\n";
    }

    return;
}

sub del_msg_subj {
    my $self = shift;
    my ($subject) = @_;

    warn "Looking for messages with Subject: $subject\n";

    # Search messages
    my $ok = 0;
    for my $msg (keys %{$self->msglist}) {
        my ($dirno, $msgno) = split(/\//, $msg);
        my $msgsub = $self->get_subject($msg);

        if ($msgsub and $msgsub =~ /$subject/) {
            $ok = 1;
            warn "Deleting message: $msgno\n";
            push @{$self->to_delete}, $msg;
        }

    }

    # If no messages are found, print a notice
    if (!$ok) {
        warn "No messages matching Subject \"$subject\" found in the queue!\n";
    }

    return;
}


# Delete all messages in the queue (thanks Kasper Holtze)
sub del_all {
    my $self = shift;

    # Search messages
    my $ok = 0;
    for my $msg (keys %{$self->msglist}) {
        $ok = 1;
        my ($dirno, $msgno) = split(/\//, $msg);
        warn "Message $msgno slotted for deletion!\n";
        push @{$self->to_delete}, $msg;
    }

    # If no messages are found, print a notice
    if (!$ok) {
        warn "No messages found in the queue!\n";
    } 

    return;
}

sub flag_remote {
    my $self = shift;
    my ($re) = @_;

    my $queue = $self->queue;

    warn "Looking for messages with recipients in $re\n";

    my $ok = 0;
    for my $msg (keys %{$self->msglist}) {
        if ($self->msglist->{$msg}{remote}) {
            open (my $msg_fh, '<', "${queue}remote/$msg")
                or die("cannot open remote file for message $msg! ",
                       "Is qmail-send running?\n");
            my $recipients = <$msg_fh>;
            chomp($recipients);
            close ($msg_fh);
            if ($recipients =~ /$re/) {
                $ok = 1;
                push @{$self->to_flag}, $msg;
                warn "Message $msg being tagged for earlier retry ",
                     "(and lengthened stay in queue)!\n"
            }
        }
    }
    # If no messages are found, print a notice
    if (!$ok) {
        warn "No messages with recipients in $re found in the queue!\n";
        return;
    }

    flag_msgs();

    return;
}

# Make statistics
sub stats {
    my $self = shift;

    my $total = 0;
    my $l = 0;
    my $r = 0;
    my $b = 0;
    my $t = 0;

    foreach my $msg (keys %{$self->msglist}) {
        $total++;
        $self->msglist->{$msg}{local}  && $l++;
        $self->msglist->{$msg}{remote} && $r++;
        $self->msglist->{$msg}{bounce} && $b++;
        $self->msglist->{$msg}{todo}   && $t++;
    }

    my $colours = $self->colours;
    my ($cstat, $cend) = @{$colours}{qw[stat end]};

   print <<"END_OF_STATS";
${cstat}Total messages${cend}: $total
${cstat}Messages with local recipients${cend}: $l
${cstat}Messages with remote recipients${cend}: $r
${cstat}Messages with bounces${cend}: $b
${cstat}Messages in preprocess${cend}: $t
END_OF_STATS
   return;
}

# Retrieve pid of qmail-send
sub qmail_pid {
    my $self = shift;
    my $pidcmd = $self->commands->{pidcmd};
    my $qmpid = `$pidcmd`;
    chomp ($qmpid);
    $qmpid =~ s/\s+//g;
    return 0 if $qmpid =~ /\D/;
    return $qmpid;
}

# Print help
sub usage {
    print <<"END_OF_HELP";
$me v$version
Copyright (c) 2016 Dave Cross <dave\@perlhacks.com>
Based on original version by Michele Beltrame <mb\@italpro.net>

Available parameters:
  -a       : try to send queued messages now (qmail must be running)
  -l       : list message queues
  -L       : list local message queue
  -R       : list remote message queue
  -s       : show some statistics
  -mN      : display message number N
  -dN      : delete message number N
  -fsender : delete message from sender
  -f're'   : delete message from senders matching regular expression re
  -Stext   : delete all messages that have/contain text as Subject
  -h're'   : delete all messages with headers matching regular expression re (case insensitive)
  -b're'   : delete all messages with body matching regular expression re (case insensitive)
  -H're'   : delete all messages with headers matching regular expression re (case sensitive)
  -B're'   : delete all messages with body matching regular expression re (case sensitive)
  -t're'   : flag messages with recipients in regular expression 're' for earlier retry (note: this lengthens the time message can stay in queue)
  -D       : delete all messages in the queue (local and remote)
  -V       : print program version

Additional (optional) parameters:
  -c       : display colored output
  -N       : list message numbers only
           (to be used either with -l, -L or -R)

You can view/delete multiple message i.e. -d123 -m456 -d567

END_OF_HELP

    exit;
}

# Print help
sub version {
    print "$me v$version\n";
    return;
}

1;