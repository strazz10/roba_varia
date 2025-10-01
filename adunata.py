#funny ispirati alla mia breve permanenza in accademia militare

import numpy as np

###########################################################################################
#"adunata" sort 1D

def adunata_sort(cadets):
	i = 0
	while i <= len(cadets)-2:
		if cadets[i+1] > cadets[i]:
			cadet_temp = cadets[i]
			cadets[i] = cadets[i+1]
			cadets[i+1] = cadet_temp
		i = i+1
	return(cadets)
	
def height_check(a):
	truth_value = np.empty(len(a)-1)
	j = 0
	while j <= len(a)-2:
		if a[j] >= a[j+1]:
			truth_value[j] = True
		else:
			truth_value[j] = False
		j = j+1
	return all(truth_value)

def azione_sort_1D(): #ordina un array 1D
	init = np.random.randint(145, 185, size=10)
	print('Aspiranti allievi ufficiali della 2° compagnia, at-tenti!', init, sep = '\n')
	temp = adunata_sort(init)
	finish = height_check(temp)
	while finish == False:
		temp = adunata_sort(temp)
		finish = height_check(temp)
		print(temp, finish)
	else:
		print('Aspiranti allievi ufficiali della 2° compagnia, ri-poso!', temp, sep = '\n')
	return temp

#azione_sort_1D()


#"adunata" sort 2D (più realistico)

def azione_sort_1D_inputable(b): #può prendere in input un array
	temp = adunata_sort(b)
	finish = height_check(temp)
	while finish == False:
		temp = adunata_sort(temp)
		finish = height_check(temp)
	else:
		print(temp)
	return temp

def azione_sort_2D(): #ordina una matrice in modo che le persone più basse stiano in basso a sx
	nrows = 6
	ncols = 5
	init = np.random.randint(145, 185, size=(nrows, ncols))
	print('Aspiranti allievi ufficiali della 2° compagnia, at-tenti!', init, sep = '\n')
	
	i = 0
	while i <= ncols-1:
		init[:,i] = azione_sort_1D_inputable(init[:,i])
		i = i+1
	
	j = 0
	while j <= nrows-1:
		init[j,:] = azione_sort_1D_inputable(init[j,:])
		j = j+1
	
	print(init)
	return init

#azione_sort_2D()
		




