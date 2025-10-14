program ising_non_square_periodico   !!da unire in uno script shell al programma python che fa l'analisi per ogni valore di beta
implicit none 

integer, parameter :: n_horiz = 5, n_vert = 5, N = 10**6                
integer, parameter :: rows = 4+3*(n_vert-1), columns = 4+3*(n_horiz-1)  
integer :: i, j, iteration, rowtemp, coltemp, s, temp1, temp2, temp3, temp4
real :: beta	          !!temperatura inversa
real :: aux, acceptance, r
integer :: lattice(rows, columns) 
double precision :: totalE, totalMagn

read *, beta

print *, rows*columns, beta

lattice = 0          !!inizializzo il reticolo 

do i=1,rows
   if (i==1 .or. mod(i,3)==1) then
   do j=1,columns
      if (j/=1 .and. mod(j,3)/=1) then
      lattice(i,j) = 1
      end if
   enddo
   else 
   do j=1,columns1
      if (j==1 .or. mod(j,3)==1) then
      lattice(i,j) = 1
      end if
   enddo
   end if
enddo

acceptance = 0       !!seleziono un punto casuale, posso saltare le prime e ultime righe/colonne
do iteration=1,N
   do i=1,rows             !!da cambiare: posso sicuramente ridurre il numero, visto che ci sono gli zeri; magari divido per 2?
      do j=1,columns
   2  call random_number(aux)
      coltemp = int(aux*(columns))+1
      call random_number(aux)
      rowtemp = int(aux*(rows))+1
      
      if (lattice(rowtemp, coltemp)==0) then   !!tengo solo i punti del reticolo effettivo (sicuramente lentissimo)
      goto 2
      end if
     
      s = 0         !!somma sui vicini, cerchiamo di renderlo periodico
      
      temp1 = mod(coltemp+1, columns)
      if (temp1==0) then
      temp1 = coltemp+1
      else if (temp1==1) then
      temp1 = 2
      end if
      
      temp2 = mod(rowtemp+1, rows)
      if (temp2==0) then
      temp2 = rowtemp+1
      else if (temp2==1) then
      temp2 = 2
      end if
      
      temp3 = rowtemp-1
      if (temp3<1) then
      temp3 = rows-1
      end if
      
      temp4 = coltemp-1
      if (temp4<1) then
      temp4 = columns-1
      end if
      
      s = s + lattice(rowtemp, temp1)
      s = s + lattice(rowtemp, temp4)
      s = s + lattice(temp2, coltemp)
      s = s + lattice(temp3, coltemp)
      s = s + lattice(temp2, temp1)
      s = s + lattice(temp2, temp4)
      s = s + lattice(temp3, temp4)
      s = s + lattice(temp3, temp1)
      
      s = s*lattice(rowtemp,coltemp)
      
      
      r = 0                     !!step metropolis
      if (s < 0) then
      lattice(rowtemp,coltemp) = -lattice(rowtemp,coltemp)
      acceptance = acceptance + 1
      else 
      call random_number(r)
      if (r <= exp(-beta*2*s)) then
      lattice(rowtemp,coltemp) = -lattice(rowtemp,coltemp)
      acceptance = acceptance + 1
      end if
      end if
      enddo
   enddo
print *, totalE(lattice, rows, columns), totalMagn(lattice, rows, columns)         !!misura dopo aver iterato su tutto il reticolo
enddo 

end program ising_non_square_periodico

function totalE(lattice, rows, columns) result(E) 
implicit none
double precision :: E
integer :: i, j, rows, columns, lattice(rows,columns), temp, aux1, aux1, aux3
temp = 0
do i=1,rows
   do j=1,columns
   aux1 = j+1
   aux2 = i+1
   aux3 = j-1
   if (j==columns) then
   aux1 = 2
   end if 
   if (i==rows) then
   aux2 = 2
   end if 
   if (i==1) then
   aux3 = rows-1
   end if 
   temp = temp - lattice(i,j)*lattice(i,aux1)
   temp = temp - lattice(i,j)*lattice(aux2,j)
   temp = temp - lattice(i,j)*lattice(aux2,aux1)
   temp = temp - lattice(i,j)*lattice(aux2,aux3)
   enddo
enddo
E = dble(temp)/dble(rows*columns)
end function totalE

function totalMagn(lattice, rows, columns) result(m) 
implicit none
double precision :: m
integer :: i, j, rows, columns, lattice(rows,columns), temp
temp = 0
do i=1,rows
   do j=1,columns
   temp = temp + lattice(i,j)
   enddo
enddo
m = dble(temp)/dble(rows*columns)
end function totalMagn
