################################################################################
#          _____ _
#         |_   _| |_  ___
#           | | | ' \/ -_)
#           |_| |_||_\___|
#                   _   _             ____            _           _
#    / \   _ __ ___| |_(_) ___ __ _  |  _ \ _ __ ___ (_) ___  ___| |_
#   / _ \ | '__/ __| __| |/ __/ _` | | |_) | '__/ _ \| |/ _ \/ __| __|
#  / ___ \| | | (__| |_| | (_| (_| | |  __/| | | (_) | |  __/ (__| |_
# /_/   \_\_|  \___|\__|_|\___\__,_| |_|   |_|  \___// |\___|\___|\__|
#                                                  |__/
#          The Arctica Modular Remote Computing Framework
#
################################################################################
#
# Copyright (C) 2015-2016 The Arctica Project 
# http://http://arctica-project.org/
#
# This code is dual licensed: strictly GPL-2 or AGPL-3+
#
# GPL-2
# -----
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; version 2 of the License.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the
# Free Software Foundation, Inc.,
#
# 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.
#
# AGPL-3+
# -------
# This programm is free software; you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This programm is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program; if not, write to the
# Free Software Foundation, Inc.,
# 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.
#
# Copyright (C) 2015-2016 Guangzhou Nianguan Electronics Technology Co.Ltd.
#                         <opensource@gznianguan.com>
# Copyright (C) 2015-2016 Mike Gabriel <mike.gabriel@das-netzwerkteam.de>
#
################################################################################
package Arctica::Core::Mother::Forker;
use strict;
use Arctica::Core::eventInit qw( genARandom BugOUT );
use Data::Dumper;
use Exporter qw(import);
## Be very selective about what (if any) gets exported by default:
our @EXPORT = qw();
## And be mindfull of what we lett the caller request here too:
our @EXPORT_OK = qw();

use POSIX qw(:sys_wait_h mkfifo);
use FileHandle;
use IO::Pty;

my $arctica_core_object;

# A BUNCH OF CLEANUP STUFF IS MISSING RIGHT NOW
# CLEANUP WHEN CHILD TERMINATES AND ON "DESTROY"
# FIX THAT SOON AND REMOVE THIS TEXT WHEN ITS DONE

sub new {
	BugOUT(9,"Mother::Forker new->ENTER");
	my $class_name = $_[0];# Be EXPLICIT!! DON'T SHIFT OR "@_";
	$arctica_core_object = $_[1];
	my $the_tmpdir = $arctica_core_object->{'a_dirs'}{'tmp_adir'};
	my $dev_conf = $_[2];
	
	if (lc($dev_conf->{'fork_style'}) =~ /^(interactive_pty)$/) {
	# There may be future options for other ways of going about forking...
	# But hopefully the interavtive_pty method will be a good universal approach for most 
		$dev_conf->{'fork_style'} = $1;
		BugOUT(9,"Mother::Forker fork_style is $dev_conf->{'fork_style'}");
	} else {
		die("Mother::Forker Unknown fork_style: '$dev_conf->{'fork_style'}'");
	}

	my $self = {
		dummydata => {# RM this junk before public release!
			somevalue	=> 321,
			otherinfo	=> "The Other INFO!",
		},
		child_name	=> $dev_conf->{'child_name'},
		fork_style	=> $dev_conf->{'fork_style'},
		isArctica	=> 1, # Declare that this is a Arctica "something"
		aobject_name	=> "MForker_Child",
	};
	
	if (-x $dev_conf->{'exec_path'})  {
		if ($dev_conf->{'exec_path'} =~ /^\//) {# Not M$ Windoz friendly... deal with that later...
			$self->{'exec_path'} = $dev_conf->{'exec_path'};
			BugOUT(9,"Mother::Forker exec_path is $self->{'exec_path'}");
		} else {
			die("We're only going to fork stuff you provide a full path for. ('$dev_conf->{'exec_path'}'!?!?)");
		}
	} else {
		die("$dev_conf->{'exec_path'} is not executable!");
	}
	
	if ($dev_conf->{'exec_cl_argv'}) {# add some sanity checks and whatnot here?
		BugOUT(9,"Mother::Forker We got command line arguments for child!");
		$self->{'exec_cl_argv'} = $dev_conf->{'exec_cl_argv'};
	}

	if ($dev_conf->{'return_stdin'} eq 1) {
		$self->{'stdin'} = 1;
	}

	if ($dev_conf->{'handle_stdeoc'}) {# combine ERR and OUT EOC = "Err+Out Combined"
		if ($dev_conf->{'handle_stdout'} or $dev_conf->{'handle_stderr'}) {
			die("You can't have a combo STEOC and individual ERR and OUT handlers");
		}
		BugOUT(9,"Mother::Forker Combining STDERR into STDOUT");
		$self->{'handle_stdeoc'} = $dev_conf->{'handle_stdeoc'};
	} else {
		if ($dev_conf->{'handle_stdout'}) {
			BugOUT(9,"Mother::Forker Individual handler for STDOUT");
			$self->{'handle_stdout'} = $dev_conf->{'handle_stdout'};
		} else {
			BugOUT(9,"Mother::Forker No handler for STDOUT");
		}
		if ($dev_conf->{'handle_stderr'}) {
			BugOUT(9,"Mother::Forker Individual handler for STDERR");
			$self->{'handle_stderr'} = $dev_conf->{'handle_stderr'};
		} else {
			BugOUT(9,"Mother::Forker No handler for STDERR");
		}
	}

	if ($dev_conf-> {'handle_death'}) {
		BugOUT(9,"Mother::Forker GoT handler for DEATH of forked child");
		$self->{'handle_death'} = $dev_conf->{'handle_death'};
	} else {
		BugOUT(9,"Mother::Forker No handler for DEATH of forked child");
	}
	$self->{'child_aid'} = genARandom('id');

	bless($self, $class_name);# Blessed be thy object.
	# Then append our self to the ACO tree:
	$arctica_core_object->{'aobj'}{'MForker'}{$self->{'child_aid'}} = \$self;
	
#	START dealing with ENV here.
	if ($dev_conf->{'env_strict'}) {
		$self->{'env_strict'} = 1;
		if ($dev_conf->{'env_pass'}) {
			$self->{'env_pass'} = $dev_conf->{'env_pass'};
		}
	}
	if ($dev_conf->{'env_set'}) {
		$self->{'env_set'} = $dev_conf->{'env_set'};
	}
#	DONE dealing with ENV here.
	
	if ($dev_conf->{'exec_hold'} eq 1) {# Not executing yet, eh?
		BugOUT(9,"Mother::Forker exec_hold requested!!");
	} else {# Executing right away, huh!?
		BugOUT(9,"Mother::Forker NO HOLD requested, executing now...");
		$self->run_child();
	}
		$self->_chkncreatedir();
	return $self;
}

sub run_child {
	BugOUT(8,"Mother::Forker about to run_child...");
	my $self = $_[0];
	
	$self->_afork_ipty();
}

sub _afork_ipty {
	my $self = $_[0];
	my $main_pty = IO::Pty->new;
	$main_pty->set_raw();# WE DONT WANT ECHOING OF WHAT WE SEND...

	my $errpipe;
	if ($self->{'handle_stderr'}) {
		 $errpipe = $self->_create_pipe($self->{'handle_stderr'});
	}

	unless ($self->{'child_pid'} = fork) {
	# START OF THE FORKING CHILD 
		$ENV{LANG} = "C";# We wan't to be dealing with the child in English
		my $slave_pty = $main_pty->slave;
		$main_pty->make_slave_controlling_terminal();
#		$main_pty->slave->clone_winsize_from(\*STDIN);
		STDOUT->autoflush(1);# These three sometimes makes a difference.
		STDERR->autoflush(1);# Other times they don't...
		STDIN->autoflush(1); # Depending on the child?!? at least causes no problems.

		my $slave_pty_fn = $slave_pty->fileno;
		open STDIN, "<&$slave_pty_fn" or die $!;

		if ($self->{'handle_stdeoc'}) {
			open STDOUT, ">&$slave_pty_fn" or die $!;
			open STDERR, ">&STDOUT" or die $!;
		} else {
			open STDOUT, ">&$slave_pty_fn" or die $!;
			if ($self->{'handle_stderr'}) {
				BugOUT(9,"Mother::Forker Sending STDERR to fifopipe $errpipe");
				open(STDERR, ">$errpipe") or die($!);
			} else {
				BugOUT(9,"Mother::Forker Sending STDERR to /dev/null");
				open(STDERR, ">/dev/null") or die($!);
			}
		}

		my @cl_args;# Move this whole following code chunk out of the child???
		if ($self->{'exec_cl_argv'}) {
			foreach my $the_arg (@{$self->{'exec_cl_argv'}}) {
				# do some sanitation here?
				push @cl_args, $the_arg;
			}
		}

#		START dealing with ENV here.
		if ($self->{'env_strict'}) {# Yay! Strict ENV!
			foreach my $key (sort keys %ENV) {
				if (($key =~ /^A\_/) and ($self->{'env_pass'}{'ARCTICA'} eq 1)) {
#					Probably do nothing here!?!
				} elsif ($self->{'env_pass'}{$key} eq 1) {
#					Probably do nothing here!?!
				} else {
					$ENV{$key} = undef;
					delete($ENV{$key});
				}
			}
		}

		if ($self->{'env_set'}) {
			foreach my $key (keys $self->{'env_set'}) {
				$ENV{$key} =  $self->{'env_set'}{$key};
			}
		}
#		DONE dealing with ENV here.

		exec($self->{'exec_path'},@cl_args);

	# END OF THE FORKING CHILD
	}
	
	if ($self->{'child_pid'} > 0) {
		BugOUT(8,"Mother::Forker child PID is: $self->{'child_pid'}");
		$self->{'glib_tnw'}{'reaper_of_zombie_children'} = Glib::Timeout->add(500, sub {\$self->_is_child_alive();}, undef, 1 );
		if ($self->{'stdin'} eq 1) {
			$self->{'stdin'} = $main_pty;
		}
		if ($self->{'handle_stdeoc'}) {
			$self->{'glib_tnw'}{'stderr_and_out'} = Glib::IO->add_watch(
				fileno( $main_pty ), ['in', 'hup','err'],
				sub {\$self->_input_reader($main_pty,$self->{'handle_stdeoc'},$_[0],$_[1],$_[2]);});
			BugOUT(9,"Mother::Forker combined STD OUT/ERR watcher initiated.");
		} else {
			if ($self->{'handle_stdout'}) {
				$self->{'glib_tnw'}{'stdout'} = Glib::IO->add_watch(
					fileno( $main_pty ), ['in', 'hup','err'],
					sub {\$self->_input_reader($main_pty,$self->{'handle_stdout'},$_[0],$_[1],$_[2]);});
					BugOUT(9,"Mother::Forker STDOUT watcher initiated.");
			} else {
				# Apparently we need to pretend to do something with STDOUT to keep everything happy...
				$self->{'glib_tnw'}{'stdout'} = Glib::IO->add_watch(
					fileno( $main_pty ), ['in', 'hup','err'],
					sub {\$self->_input_reader($main_pty,sub {},$_[0],$_[1],$_[2]);});
					BugOUT(9,"Mother::Forker watching STDOUT but doing nothing.");
			}
#			if ($self->{'handle_stderr'}) {
#				This is already being taken care of by the pipe plumber..
#				Do nothing here? remove this chunk before release?!!?
#				print "INSERT ERRPIPE STUFF HERE!\n";
#			}
		}
	} else {
		warn("FAIL TO FORK!");# The application using this module may want to try to recover from this.. or not?!?
	}
}

sub _is_child_alive {# Idealy you should catch deaths before this do...
	# Could this be done more effichient and pretty?
	my $self = $_[0];
	my $thekill = kill(0,$self->{'child_pid'});
	if ($thekill > 0) {
		foreach my $zpid (ZombieSlayer()) {
			$zpid =~ s/\D//g; 
			if ($zpid eq $self->{'child_pid'}) {
				BugOUT(8,"Mother::Forker child appear to be dead...");
				$self->limpiador_de_la_muerte();
			}
		}
	} else {
		# insert the call for cleanup here too?
	}
}


sub limpiador_de_la_muerte {
	my $self = $_[0];
	if ($self->{'dead'} ne 1) {# Death may be noticed multiple places...
		$self->{'dead'} = 1;# Prevents multiple cleanup instances...
		my $death_handler;

		if ($self->{'handle_death'}) {
			$death_handler = $self->{'handle_death'};
		}

#		Glib::Source->remove($self->{'glib_timeout'});# THESE TWO NEED TO GO...
#		Glib::Source->remove($self->{'glib_watcher'});# foreach of $self->{'glib_tnw'} instead...
# 		Not that it's really needed.... explicitly cleaning shit up never hurt nobody...

		if ($death_handler) {
			$death_handler->("AAAAAAAWWWWWWWWWWWWWW WERE DEAD!!!");
		}
		return 0;
	}
}

sub send {
	my $self = $_[0];
	if ($self->{'stdin'} and ($self->{'dead'} ne 1)) {
		my $bytes_written = syswrite($self->{'stdin'},"$_[1]\n") or warn("Mother::Forker fail to write to childs STDIN");
		$self->{'stdin'}->flush;# REDUNDANT!?
		if ($bytes_written > 0) {
			BugOUT(9,"Mother::Forker  $bytes_written bytes Written to childs STDIN")
		} else {
			if (lenght($_[1]) > 0) {
				warn("Mother::Forker fail to write to childs STDIN");
			} else {
				warn("Mother::Forker WTF?!");
			}
		}
	} else {
		warn("Mother::Forker write: Did not write a damn thing!");
	}
}

sub _input_reader {
	my $self = $_[0];
	my $read_fd = $_[1];
	my $handler_function = $_[2];
#	print "$read_fd\t$_[3]\t$_[4]\t$_[5]\n\n";# don't forget to either remove this or turn it into a bugout
	my $buffer;
	my $bytesread = sysread($read_fd,$buffer,8192);
	if ($bytesread > 0) {
		foreach my $line (split(/\n/, $buffer)) {
			$line =~ s/\n//g;
			unless ($line =~ /^\s*$/) {
				$handler_function->($line);
			} else {
#				print "GOT EMPTY LINE\n";
			}
		}
	}
	return 1;
}

sub _create_pipe {
	my $self = $_[0];
	my $handler_function = $_[1];
	if ($handler_function) {
		my $fullpath2 = $self->_chkncreatedir();
		my $pipeid = genARandom('id');
		my $pipeFH = FileHandle->new;
		mkfifo("$fullpath2/$pipeid.fifo",0700);
		open($pipeFH,"+<$fullpath2/$pipeid.fifo");
		$pipeFH->autoflush(1);
		$self->{'glib_tnw'}{"fifo_$pipeid"} = Glib::IO->add_watch($pipeFH->fileno, [qw/in/],
			sub {\$self->_input_reader($pipeFH,$handler_function,$_[0],$_[1],$_[2]);} ,$pipeFH);

		return "$fullpath2/$pipeid.fifo";
	} else {
		die("Mother::Forker did you forget to specify a handler func?");
	}
}

sub log_slurper {# create pipe we can declare as log file.
	my $self = $_[0];
	my $handler_function = $_[1];
	if ($handler_function) {
		unless (($self->{'dead'} eq 1) and ($self->{'child_pid'} > 0)) {
			print "SLURP!!!!!!!!!!!!!! NOM NOM NOM.....\n";
			my $pipe_path = $self->_create_pipe($handler_function);
			if (-p  $pipe_path) {
				return  $pipe_path;
			} else {
				die("Mother::Forker Unable to create pipe");
			}
		} else {
			die("Mother::Forker Slurper must be initated before exec!");
		}
	}
}


sub clargv_append {
	my $self = $_[0];
	my $new_clargv = $_[1];
	unless ($new_clargv =~ /^\s*$/) {
		unless (($self->{'dead'} eq 1) and ($self->{'child_pid'} > 0)) {
			push $self->{'exec_cl_argv'}, $new_clargv;
		} else {
			die("Mother::Forker Must append cl_argv before exec!");
		}
	}
}


sub new_blank_file {
	my $self = $_[0];
	my $options = $_[1];
	my $full_path = $self->_chkncreatedir('files');
	if (-d $full_path) { 
		my $file_id = genARandom('id');
		my $fext = "file";
		if ($options->{'_configfile'} eq 1) {
			$fext = "conf";
		}
		if (($options->{'_configfile'} ne 1) and ($options->{'create'} eq 1)) {
			open(EMPTY,">$full_path/$file_id.$fext") 
				or die("Mother::Forker Can't create file: '$full_path/$file_id.$fext' ($!)");
			close(EMPTY);
			BugOUT(9,"Mother::Forker created empty file: '$full_path/$file_id.$fext'");
		} else {
			BugOUT(9,"Mother::Forker got path for new file: '$full_path/$file_id.$fext'");
		}
		
		if (lc($options->{'name'}) =~ /^([a-z]{1,16})$/) {
			$self->{'Files'}{$file_id}{'name'} = $1;
		}
		$self->{'Files'}{$file_id}{'path'} = "$full_path/$file_id.$fext";
		$self->{'Files'}{$file_id}{'fext'} = $fext;
		
		return "$full_path/$file_id.$fext";
	} else {
		die("Mother::Forker Directory '$full_path' does not exist?!");
	}
}

sub new_empty_dir {
	my $self = $_[0];
	my $options = $_[1];
	my $full_path = $self->_chkncreatedir('dirs');
	if (-d $full_path) {
		my $dir_id = genARandom('id');
		if (lc($options->{'name'}) =~ /^([a-z]{1,16})$/) {
			$self->{'Dirs'}{$dir_id}{'name'} = $1;
		}
		mkdir("$full_path/$dir_id") 
			or die("Mother::Forker Can't create dir '$full_path/$dir_id' ($!)");
		$self->{'Dirs'}{$dir_id}{'path'} = "$full_path/$dir_id";
		BugOUT(9,"Mother::Forker created empty dir '$self->{'Dirs'}{$dir_id}{'path'}'");
		return "$full_path/$dir_id";
	} else {
		die("Mother::Forker Directory '$full_path' does not exist?!");
	}
}


sub cfg_file_init {
	my $self = $_[0];
	my $options = $_[1];
	if ($options->{'blob'}) {# complete blob
		BugOUT(9,"Mother::Forker cfg_file_init BLOB");
#		This will be for when you'd want to just supply a whole config file blob 
#		rather than using the config template functions
		die("BLOB CONF Not yet!!");
	} elsif ($options->{'template'}) {# use template 
		if ((-f $options->{'template'}) and ($options->{'template'} =~ /^\//)) {
			BugOUT(9,"Mother::Forker cfg_file_init TEMPLATE: '$options->{'template'}'");
			open(ACNFT,$options->{'template'});
			my ($first_line,@file_content) = <ACNFT>;
			close(ACNFT);
			if (uc($first_line) =~ /ACNFT\_START/) {
				my $cfg_id = genARandom('id');
#				$self->{'cfg_files'}{$cfg_id}{'pdv'}
#				my %decvars;
				my $var_dec;
				($var_dec,$self->{'cfg_files'}{$cfg_id}{'template'},undef) = split(/\#{6,}\n/,"@file_content");
				foreach my $varline (split(/\n/,$var_dec)) {
					$varline =~ s/^\s*//g;
					$varline =~ s/\s*$//g;
					if ($varline =~ /^(\d)\;([a-z0-9]{1,16})\;([A-Z0-9\_]{3,32})\=(.*)$/) {
						$self->{'cfg_files'}{$cfg_id}{'pdv'}{$3}{'value'} = $4;
						if ($1 eq 0) {
							$self->{'cfg_files'}{$cfg_id}{'pdv'}{$3}{'req'} = 0;
						} else {
							$self->{'cfg_files'}{$cfg_id}{'pdv'}{$3}{'req'} = 1;
						}
						
						if ($2 eq 0) {
							$self->{'cfg_files'}{$cfg_id}{'pdv'}{$3}{'sanitation'} = 0;
						} else {
#							ADD SOME SANITATION STUFF HERE?
							die("Mother::Forker cfg_file_init '$2' is not a valid sanitation rule");
						}
					} else {
						if (length($varline) > 0) {
							warn("Mother::Forker cfg_file_init not a valid vardec: '$varline'");
#							print "NOT A VALID VARDEC LINE?!\n";
						}
					}
				}
#				@file_content = undef;$var_dec = undef;
				$self->{'cfg_files'}{$cfg_id}{'file_path'} = $self->new_blank_file({
						_configfile	=> 1,
						create		=> 0,
					});
				$self->_cfg_file_template_prep_vars($cfg_id,$options->{'vars'},1,0);
				
				if ($options->{'handle_cfg_change'}) {
					$self->{'cfg_files'}{$cfg_id}{'handle_cfg_change'} = $options->{'handle_cfg_change'};
				}
				
				return $cfg_id;

			} else {
				die("Mother::Forker '$options->{'template'}' does not appear to be a ACNFT file?!?");
			}
			
			
			 
		} else {
			die("Mother::Forker cfg template must be full path to existing file!");
		}
	} else {
		die("Mother::Forker cfg file must be either 'blob' or 'template'!");
	}
}

sub cfg_file_chval {# update existing config file.... maybe trigger some signaling?
	my $self = $_[0];
	my $cfg_id = $_[1];
	my $new_confv = $_[2];
	if ($self->{'cfg_files'}{$cfg_id}) {
		if (($self->{'cfg_files'}{$cfg_id}{'template'}) and (ref($new_confv) eq "HASH")) {
			BugOUT(9,"Mother::Forker cfg_file_chval template style.");
			$self->_cfg_file_template_prep_vars($cfg_id,$new_confv,1,1);
		} elsif ($self->{'cfg_files'}{$cfg_id}{'blob'}) {
			BugOUT(9,"Mother::Forker cfg_file_chval blob style.");
			die("Mother::Forke blobs not supportet ATM.. :-(");
		} else {
			die("Mother::Forker something is probably messed up!");
		}
	} else {
		warn("Mother::Forker '$cfg_id' is not a valid cfg file id");
	}
#	if (reg($new_values) eq "HASH")
#mostly relying on _cfg_file_template_prep_vars for template based files 
#conf blobs not a pri at the moment 
}

sub _cfg_file_template_prep_vars {
	my $self = $_[0];
	my $cfg_id = $_[1];
	my $in_vars = $_[2];
	my $writefile = $_[3];
	my $trigger_call = $_[4];
	if ($self->{'cfg_files'}{$cfg_id}{'template'}) {
			foreach my $inv_key (keys $in_vars) {
#				if (length($in_vars->{$inv_key}) > 0) {# need to allow empty values
				my $uc_key = uc($inv_key);
				if ($self->{'cfg_files'}{$cfg_id}{'pdv'}{$uc_key}) {
					if ($self->{'cfg_files'}{$cfg_id}{'pdv'}{$uc_key}{'sanitation'} eq 0) {
						unless (($self->{'cfg_files'}{$cfg_id}{'pdv'}{$uc_key}{'req'} eq 1) and (length($in_vars->{$inv_key}) < 1)) {
							$self->{'cfg_files'}{$cfg_id}{'pdv'}{$uc_key}{'value'} = $in_vars->{$inv_key};
						} else {
							warn("Mother::Forker _cfg_file_template_prep_vars required var '$inv_key' can not be set to empty value!");
						}
					} else {
#						Insert some santiation functions here at some point?
						die("BUT FOR NOW... WE DIE HERE!");
					}
				} else {
					warn("Mother::Forker invalid cfg var '$inv_key'!");
				}
#				}# need to allow empty values
			}


		if (($writefile eq 1) and ($self->{'cfg_files'}{$cfg_id}{'file_path'} =~ /^\//)) {
			my $template = $self->{'cfg_files'}{$cfg_id}{'template'};
			foreach my $key (keys $self->{'cfg_files'}{$cfg_id}{'pdv'}) {
				if ($template =~ /\$\$ACV\-$key\$\$/) {
#					my $value = $decvars{$key}{'value'};
#					print "FOUND MATCH: $1 $value\n";
					unless (($self->{'cfg_files'}{$cfg_id}{'pdv'}{$key}{'req'} eq 1) and (length($self->{'cfg_files'}{$cfg_id}{'pdv'}{$key}{'value'}) < 1)) {
						$template =~ s/\$\$ACV\-$key\$\$/$self->{'cfg_files'}{$cfg_id}{'pdv'}{$key}{'value'}/g
					} else {
						warn("Mother::Forker _cfg_file_template_prep_vars required var '$key' can not be set to empty value!");
					}

				} else {
					warn("Mother::Forker _cfg_file_template_prep_vars no match in template for '$key'!");
				}
			}

			if ($template =~ /\$\$ACV\-[A-Z0-9\_]{3,32}\$\$/) {
				$template =~ s/\$\$ACV\-[A-Z0-9\_]{3,32}\$\$//g;
				warn("Mother::Forker _cfg_file_template_prep_vars leftover placeholder(s) in template!");
			}

			open(CFG,">$self->{'cfg_files'}{$cfg_id}{'file_path'}") 
				or die("Mother::Forker _cfg_file_template_prep_vars UNABLE to write to file '$self->{'cfg_files'}{$cfg_id}{'file_path'}' ($!)");
			foreach my $cfg_line (split(/\n/, $template)) {
				$cfg_line =~ s/^\s*//g;
				$cfg_line =~ s/\s*$//g;
				unless ($cfg_line =~ /^\s*$/) {
					print CFG "$cfg_line\n";
				}
			}
			close(CFG);

			if (($trigger_call eq 1) and ($self->{'cfg_files'}{$cfg_id}{'handle_cfg_change'})) {
				$self->{'cfg_files'}{$cfg_id}{'handle_cfg_change'}($cfg_id);
			}
		}

	} else {
		if ($self->{'cfg_files'}{$cfg_id}) {
			die("Mother::Forker '$cfg_id' is not a valid cfg_id?!");
		} else {
			die("Mother::Forker '$cfg_id' is not a template style cfg file!?!");
		}
	}
}

sub _chkncreatedir {
	my $self = $_[0];
	my $subdir = $_[1];
	# Figure out what class this application is and create in appropraite place.
	# DONT FORGET TO SET STRICT FILE PERMS (WE GOT A SUB FOR THAT!!)
	my $full_path = $arctica_core_object->{'a_dirs'}{'tmp_adir'};
	unless (-d $full_path) {
		mkdir($full_path) or die("Could not create directory: '$full_path' ($!)");
	}
	
	$full_path .= "/foobar";# This is probably where we would want to so something based on application class.
	unless (-d $full_path) {
		mkdir($full_path) or die("Could not create directory: '$full_path' ($!)");
	}
	
	$full_path .= "/$arctica_core_object->{'AExecDeclaration'}{'self_aID'}.apd";
	unless (-d $full_path) {
		mkdir($full_path) or die("Could not create directory: '$full_path' ($!)");
	}
	
	$full_path .= "/children";
	unless (-d $full_path) {
		mkdir($full_path) or die("Could not create directory: '$full_path' ($!)");
	}
	
	$full_path .= "/$self->{'child_aid'}.cld";
	unless (-d $full_path) {
		mkdir($full_path) or die("Could not create directory: '$full_path' ($!)");
	}
	
	if ($subdir eq "files") {
		$full_path .= "/files/";
		unless (-d $full_path) {
			mkdir($full_path) or die("Could not create directory: '$full_path' ($!)");
		}
	} elsif ($subdir eq "dirs") {
		$full_path .= "/dirs/";
		unless (-d $full_path) {
			mkdir($full_path) or die("Could not create directory: '$full_path' ($!)");
		}
	}
	
	if (-d $full_path) {
		return $full_path;
	} else {
		return 0;
	}
}

#sub _check_n_remove_dir {	$self = $_[0];}


sub signal {
	my $self = $_[0];
	my $signal = uc($_[1]);
# signal the child if its running...
# Obviously we'll do something with "kill" here...
# playing with a few options.
	if (($signal =~ /^(\-?[A-Z]*)$/) or ($signal =~ /^(\-?\d*)$/)) {
		$signal = $1;
		# add some future signal sanitation?
		if ($self->{'child_pid'}) {
			if (kill(0,$self->{'child_pid'}) and ($self->{'dead'} ne 1)) {
				BugOUT(9,"Mother::Forker sending '$signal' to PID#$self->{'child_pid'}");
				kill($signal,$self->{'child_pid'}) 
					or warn("Mother::Forker unable to signal child PID#$self->{'child_pid'} ($!)");
			} else {
				warn("Mother::Forker unable to signal child PID#$self->{'child_pid'} ($self->{'dead'})");
			}
		} else {
			warn("Mother::Forker NO child PID? So... we're unable to signal child, did it ever run?)");
		}
	} else {
		die("Mother::Forker signal '$signal' is invalid or not supported.");
	}


}

sub ZombieSlayer {# This one may be usefull for external use too...
#	usleep(1000);# Give stuff a remote chance of actually being zombified... and if not.... not really a big deal..
	my $zombID = 1;
	my $maxCnt = 0;# Just for those unforseen, unlikely events when '$zombID'  would never free us from the while loop. 
	my @zombieList;
	while (($zombID > 0) and ($maxCnt < 100)) {
		$maxCnt++;
		$zombID = waitpid(-1, WNOHANG);
		if ($zombID > 0) {
			push @zombieList, $zombID;
		}
	}
	return @zombieList;
}

sub DESTROY {
	my $self = $_[0];
	warn("Mother::Forker DESTROY object $self->{'child_id'}");
}


1;
