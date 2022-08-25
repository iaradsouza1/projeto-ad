#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug 24 09:40:38 2022

@author: clovis
"""

import os
import shutil

dir = 'M2'
os.chdir('/home/clovis/lixo/teste/'+dir)

#crie a lista de arquivos do diretório e remova os espaços com ls -l|cut -f 9- -d ' '|sed 's/ //g'
#correlacione cada ID com o nome do diretório
if dir == 'M1':
    arq = [["Ind1","AdelaideHirano"],["Ind2","AlcidesMininel"],["Ind3","AlietedeSouzaFiori"],["Ind4","AntonioRobertodeLucca"],["Ind5","AparecidaDiasBotelho"],["Ind6","BentaLinsAndrade"],["Ind7","CeliaBorges"],["Ind8","CéliaCaveque"],["Ind9","DelciAmélia"],["Ind10","DulcineiaJacintoPedrolongo"],["Ind11","IldaBorg"],["Ind12","IneaTeixeiraMachado"],["Ind13","JoséAlvesPereira"],["Ind14","JuventinaApdeSouza"],["Ind15","MariadeFátimaCaputo"],["Ind16","MariaJosePereiraOliveiraFerreira"],["Ind17","MariaJuliaBonaniJavarotti"],["Ind18","MariaLuciaConstantinoPineli"],["Ind19","MercedesRomãoPires"],["Ind20","MiguelGregorioCovacs"],["Ind21","NadirAnanias"],["Ind22","NagibSabe"],["Ind23","NailzaIsabeldeBritoPaz"],["Ind24","NairTSalomão"],["Ind25","NelsonLopes"],["Ind26","OdeteApCandiloraMozan"],["Ind27","PalmiraSouzaAnanias"],["Ind28","PaschoalGeraldo"],["Ind29","PrimianoFrancisco"],["Ind30","ReginaRosolin"],["Ind31","SoniaMarlyFrigo"],["Ind32","VicentedePaulaMartins"],["Ind33","WolfgangRodolfo"]]
else:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
    arq = [["Ind1","AdelaideHirano"],["Ind2","AlcidesMininel"],["Ind3","AlietedeSouzaFiore"],["Ind4","AntonioRobertodeLuca"],["Ind5","AparecidaDiasBotelho"],["Ind6","BentaLindAndrade"],["Ind7","CeliaBorges"],["Ind8","CéliaMariadaSilvaCavéquia"],["Ind9","DelciAmeliaLeonardoGolnçalvez"],["Ind10","DulcineiaJacintoPedroLongo"],["Ind12","IneaTeixeiraMachado"],["Ind13","JoséAlvesPereira"],["Ind14","JuventinaApdeSouza"],["Ind15","MariadeFátimaCaputo"],["Ind16","MariaJosePereiradeOliveiraFerreira"],["Ind17","MariaJuliaBonanijavarotti"],["Ind18","MariaLuciaConstantinoPineli"],["Ind20","MiguelCovacs"],["Ind21","NadirAnanias"],["Ind23","NailzaIsabelBritoPaz"],["Ind24","NairTirapelleSalomão"],["Ind25","NelsonLopes"],["Ind27","PAlmiradeSouzaAnaninas"],["Ind28","PaschoalGeraldoGibelato"],["Ind29","PrimianoFrancisco"],["Ind30","ReginaRosalemGomes"],["Ind31","SoniaMarliBonicelliFrigo"],["Ind32","VicentedePaulamartins"],["Ind33","WolfgangFallang"]]

idx = 30
for idx in range(0,len(arq)):
    #os.rename(arq[idx][1], arq[idx][0])
    # try:
    os.chdir('./'+arq[idx][1])
    # except:
    #     next
    oName = os.listdir('./')
    sName = [item.split('_') for item in oName]
    idx2 = 0
    for idx2 in range(0,len(sName)):
        if len(sName[idx2]) == 5:
            dirName = sName[idx2][2]+sName[idx2][3]
            nName= arq[idx][0]+'_'+dirName+'_'+sName[idx2][4]
        elif len(sName[idx2]) == 4:
            dirName = sName[idx2][2]
            nName= arq[idx][0]+'_'+dirName+'_'+sName[idx2][3]
        else:
            next
        if not os.path.exists('../'+dirName):
            os.mkdir(('../'+dirName))
        print(arq[idx][0], oName[idx2],'../'+dirName+'/'+nName)
        shutil.move(oName[idx2],'../'+dirName+'/'+nName)   
                
    os.chdir('../')
    # try:
    os.rmdir('./'+arq[idx][1])
    # except:
    #     next
        
    #ATENÇÃO:
    #Ao término do processo é necessária uma reorganização manual do conteúdo dos diretórios criados, devido a falta de padrão encontada nos nomes dos arquivos de dados.
