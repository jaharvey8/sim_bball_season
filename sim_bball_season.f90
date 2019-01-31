program sim_bball_season
!************************************************************************
!
! Fortran program for simulating the rest of the fantasy basketball 
! and calculating the probability each team makes the playoffs. Winners
! of matchups are determined via a coinflip. Ties, although theoretically,
! possible, are not allowed in this model. Essentially if the season
! is simulated "enough" times this code determines the percentage of
! possibilities that exist in which a team can make the playoffs. 
!
! Author - Jacob A. Harvey
!
!************************************************************************

       use kinds

       implicit none

       integer(kind = ip) :: i,nteams,nwks,nsim,total_wins,total_losses
       integer(kind = ip) :: winner,loser,best_team(1),nplayoffs,j
       integer,dimension(33) :: seed
       integer(kind = ip),dimension(14) :: wins,losses,rank,final_standings
       integer(kind = ip),dimension(35,2) :: matchup
     
       real(kind = dp) :: num,best_win_pct
       real(kind = dp),dimension(14) :: pts,final_win_pct

       character(len=80),dimension(14) :: team_name

       logical,dimension(14) :: mask

! get inputs and initialize stuff

       open(unit = 1 , file = 'sim-bball-season.in')
       read(1,*)
       read(1,*) nteams , nwks , nsim
       close(unit = 1)

       nplayoffs = 0
       seed = 12341325
       call random_seed(PUT = seed)


! read in the remaining matches

       open(unit = 1 , file = 'matches.txt')

       do i = 1 , ((nteams * nwks) / 2)

            read(1,*) matchup(i,1) , matchup(i,2)

       enddo

       close(unit = 1)

! loop over number of times to sim season

       do j = 1 , nsim

!            write(*,*) "Sim Number: " , j
!            write(*,*) ""
! read in current standings

            open(unit = 1 , file = 'standings.txt')
            read(1,*)

            do i = 1 , 14
   
                 read(1,*) rank(i) , team_name(i) , wins(i) , losses(i) , pts(i)

            enddo

            close(unit = 1)

! simulated each match as a coinflip

            do i = 1 , ((nteams * nwks) / 2)

                 call random_number(num)

!                 write(*,*) num

! if number is less than .5 team 1 is the winner, team 2 is the loser

                 if (num .lt. .50_dp) then

!                      write(*,*) "In matchup: " , i , " team 1 won"
                      winner = matchup(i,1)
                      loser = matchup(i,2)
        
! if number is greater than or equal to .5 then team 2 is the winner and team 1 is the loser

                 else if (num .ge. .50_dp) then

!                      write(*,*) "In matchup: " , i , " team 2 won"
                      winner = matchup(i,2)
                      loser = matchup(i,1)

                 endif

! increment wins of winner and losses of loser

                 wins(winner) = wins(winner) + 1
                 losses(loser) = losses(loser) + 1

            enddo

            total_wins = sum(wins)
            total_losses = sum(losses)

            if (total_wins .ne. total_losses) then

                 write(*,*) "Error: total wins ne to total losses"
                 stop
   
            endif

! calculate final standings

            do i = 1 , 14 

                 final_win_pct(i) = real(wins(i)) / (real(wins(i)) + real(losses(i)))
!                 write(*,*) i , wins(i) , losses(i) , final_win_pct(i)

            enddo

! sort based on final winning pct

            mask = .true.

            do i = 1 , 14
  
                 best_win_pct = maxval(final_win_pct, mask=mask)
                 best_team = maxloc(final_win_pct, mask)

                 final_standings(i) = best_team(1)

                 mask(best_team) = .false.
!                 write(*,*) best_win_pct , best_team

            enddo

! determine if team 12 made the playoffs

            do i = 1 , 14

                 if ((final_standings(i) .eq. 12) .and. i .lt. 9) nplayoffs = nplayoffs + 1

            enddo

       enddo

       write(*,*) "Team 12 made the playoffs: ", nplayoffs , " times"

       stop

end program sim_bball_season
