
#Epic Seven Epic Gear Score Simulation Study / 영웅템 기준 장비점수 구하기


#--------------------note to self-------------
#1. 장비를 만든다
#2. 장비만든 기본스탯 저장, 기본 총합 저장
#3. 장비 강화 9강, 총합 저장
#4. 장비강화 15강, 총합 저장

#atk=4:8
#hp_p=4:8
#hp_f=147:202
#eff=4:8
#res=4:8
#crr=3:5
#crd=3:7
#spd=2:4

#깡공 110당 10%, 깡생 200에 4% (500에 10%), 깡방 30에 4.2% (71에 10%)

#-------------------------Create Data Structure / 데이터 스트럭처 구축축-------------------------------------

creategear=function(removed){ #takes indexes of what are kept / 아래의 부옵 중 킵하는걸 가져옴
  #base option names / 기본옵 이름
  gstats_all=c('atk_f','atk_p','critr','critd','speed','hea_p','hea_f','effec','resis', 'def_p', 'def_f') #11 options total
  gstats=gstats_all[removed]
  
  #names for +9 / 9강 이름
  gstats_nine=NULL
  for(i in 1:(length(gstats))){
    gstats_nine[i]=paste(gstats[i],'_nine', sep='')
  }

  #names for +15 / 15강 이름 
  gstats_full=NULL
  for(i in 1:(length(gstats))){
    gstats_full[i]=paste(gstats[i],'_full', sep='')
  }

  # # of rolls / 횟수 변수 생성
  rolls=NULL
  for(i in 1:(length(gstats))){
    rolls[i]=paste(gstats[i],'_roll', sep='')
  }
  #장비점수 변수
  scores=c('score_base', 'score_off_base', 'score_dta_base', 'score_def_base', 'score_uti_base', 
           'score_nine', 'score_off_nine', 'score_dta_nine', 'score_def_nine', 'score_uti_nine', 
           'score_full', 'score_off_full', 'score_dta_full', 'score_def_full', 'score_uti_full')
  allstats=c(gstats, gstats_nine, gstats_full, rolls, scores)
  
  gdata=data.frame(matrix(ncol=length(allstats), nrow=0))
  colnames(gdata)=allstats
  return(gdata)
}

#-----------------------Calculate gear score/장비점수계산-----------------------------------
gearscore=function(gear, numops){ #takes whole dataset and number of options / 전체 데이터셋, 총 부옵갯수
  for(i in 1:nrow(gear)){
    startindex=1
    endindex=numops
    scorestart=numops*4+1
    scoreend=numops*4+5
    for(p in 1:3){ #Repeats 3 times: base, +9, +15/기본장비, 9강, 15강 3번반복
      sum_all=0
      sum_off=0
      sum_def=0
      sum_uti=0
      sum_dta=0
      for(j in startindex:endindex){
        score=gear[i,j]
        opname=substr(names(gear)[j],1,5)
        score=ifelse(opname=='speed',score*2,score) #speed score is doubled / 속도는 2배
        score=ifelse(opname=='critr',score*1.5,score) #crit rate score is 1.5 / 치확은 1.5배
        #cat('score to add is ', score,'\n') #print check
        if(opname=='speed'||opname=='atk_p'||opname=='atk_f'||opname=='critr'||opname=='critd'){
          sum_off=sum_off+score
        }
        if(opname=='speed'||opname=='hea_f'||opname=='hea_p'||opname=='def_p'||opname=='def_f'||opname=='resis'){
          sum_def=sum_def+score
        }
        if(opname=='speed'||opname=='hea_f'||opname=='hea_p'||opname=='def_p'||opname=='def_f'||opname=='effec'){
          sum_uti=sum_uti+score
        }
        if(opname=='speed'||opname=='atk_p'||opname=='atk_f'||opname=='critr'||opname=='critd'||
           opname=='hea_f'||opname=='hea_p'||opname=='def_p'||opname=='def_f'){
          sum_dta=sum_dta+score
        }
        sum_all=sum_all+score
      }
      scores_combined=c(sum_all, sum_off, sum_dta, sum_def, sum_uti)
      #print(scores_combined) #check
      gear[i, scorestart:scoreend]=scores_combined
      startindex=startindex+numops
      endindex=endindex+numops
      scorestart=scorestart+5
      scoreend=scoreend+5
    }
    
  }
  return(gear)
}


#-----------------------Value Up/처음 지정, 강화-------------------------------
valup=function(a){ #takes string value, returns number / 부옵이름을 가져오고 수치를 반환
  val=NULL
  if(a=='atk_p'||a=='hea_p'||a=='effec'||a=='resis'||a=='def_p'){
    val=sample(4:8, 1)
  }
  else if(a=='speed'){
    val=sample(2:4,1)
  }
  else if(a=='critr'){
    val=sample(3:5,1)
  }
  else if(a=='critd'){
    val=sample(3:7,1)
  }
  else if(a=='atk_f'){
    val=sample(30:47,1)/11 #11당 1%
  } 
  else if(a=='def_f'){
    val=sample(25:34,1)/7.1 #7.1당 1%
  }
  else if(a=='hea_f'){
    val=sample(147:202,1)/50 #50당 1%
  }
  #cat('value is', val, '\n') #checkup
  return(val)
}

#-------------------------Sample Gears----------------------------
samplegears=function(gears, numops){ #takes in gear dataset and # of options / 데이터셋과 부옵 수를 가져옴
  gnames=names(gears) #gear dataset의 이름만 기억

  for(i in 1:10000){ #repeats 10000 times / 10000번 반복
    ops=sample(1:numops, 4, replace=F) #Chooses 4 options/옵션으로 들어갈 4개를 고름
    gear=rep(0, (numops*4+15))
    #-------------------------------------Create/제작----------------------
    for(j in 1:3){ #only 3 goes in at first/처음에 3개만 들어감
      #cat('in forloop:', gnames[ops[j]], ', element:', ops[j], ' ') #checkup
      inival=valup(gnames[ops[j]])
      gear[ops[j]]=inival
    }
    gear[(numops+1):(numops*2)]=gear[1:numops]
    #cat(gear) #checkup

    #-------------------------------------9강-----------------------------
    rolls_nine=sample(ops[-4], 3, replace=T) #chooses 3 options to enhance except 4th op/4번째 옵션 외 3개 강화할거 고름
    #print(rolls_nine)
    for(p in 1:3){
      enhval=valup(gnames[rolls_nine[p]])
      gear[rolls_nine[p]+numops]=gear[rolls_nine[p]+numops]+enhval
      #cat(p,'번째 강화, 'gear[rolls_nine[p]+numops],'+',enhval,'해서') #checkup
      gear[rolls_nine[p]+(numops*3)]=gear[rolls_nine[p]+(numops*3)]+1
    }
    gear[(numops*2+1):(numops*3)]=gear[(numops+1):(numops*2)]

    #-----------------------------------12강-----------------------------
    gear[ops[4]+(numops*2)]=valup(gnames[ops[4]])
    #cat(gnames[ops[4]],'가 추가됨\n') #checkup
    #-----------------------------------15강---------------------------
    roll_full=sample(ops,1)
    enhval2=valup(gnames[roll_full])
    gear[roll_full+(numops*2)]=gear[roll_full+numops]+enhval2
    gear[roll_full+(numops*3)]=gear[roll_full+(numops*3)]+1
    #----------------------------------Adding Gear/장비추가-------------------------
    gears[nrow(gears)+1,]=gear
  }
  return(gears)
}

#--------------------------MAIN PROCESS / 실험실행-----------------------------------
set.seed(123)
weap=creategear(c(-1, -10, -11)) #Weap:-AtkFlat, DefFlat, DefRate / 무기: -깡공, 깡방, 방퍼
dim(weap) #8 possibilities, 32+15
weapons=samplegears(weap, 8)
weapons=gearscore(weapons, 8)

#weapons[,1:8] #list first 8 (base) values
#sum(weapons[,6]>0) #깡생 42개
#sum((weapons[,4]>0 & weapons[,5]>0)) #생퍼 속도있는게 13개개
#weapons[,1:16] #15강 
#weapons[,25:32] #강화횟수
#weapons[33:35]
mean(weapons$score_full)
max(weapons$score_full)
sum(weapons$score_full>55) #장비점수 55점 이상이 84개
#length(which(weapons$score_full>55)) #same result
bitik=which(weapons$score_full>55)
sum(weapons$score_off_full>55) #공갓템 2개
sum(weapons$score_def_full>50) #방어갓템 12개개
which(weapons$score_def_full>55)
sum(weapons$score_uti_full>55) #유틸0개
sum(weapons$score_dta_full>55) #딜탱갓템 11개

max(weapons$score_full)#min=26.2, max=59.5
#히스토그램
hist(weapons$score_full, xlab='장비점수', ylab='갯수', 
     main='영웅등급 무기 시뮬 장비점수, N=10000', xlim=c(20,70), col='#33CCCC', cex.lab=1.5)
#최종 장비점수 55 이상의 장비는 9강때 몇점일까?
mean(weapons[bitik,]$score_nine) #42.620
#비틱가능성템 최소컷
min(weapons[bitik,]$score_nine) #39.82
#평균 9강 점수
mean(weapons$score_nine) #33.38

#------------------------투구--------------
set.seed(333)
helm=creategear(c(-7)) #-깡생
dim(helm) #10 possibilities+15
helmets=samplegears(helm, 10)
helmets=gearscore(helmets, 10)
#결과는?
mean(helmets$score_full) #43.44
max(helmets$score_full) #61.5
sum(helmets$score_full>55) #장비점수 55점 이상이 63개
sum(helmets$score_def_full>50) #방어21개
sum(helmets$score_dta_full>55) #딜탱 14개
sum(helmets$score_uti_full>55) #유틸 1개
sum(helmets$score_off_full>50) #50점부터 11개
#히스토그램
hist(helmets$score_full, xlab='장비점수', ylab='갯수', 
     main='영웅등급 투구 시뮬 장비점수, N=10000', xlim=c(20,70), col='#CC99FF', cex.lab=1.5)
#최종 장비점수 55 이상의 장비는 9강때 몇점일까?
bitik_helm=which(helmets$score_full>55)
mean(helmets[bitik_helm,]$score_nine) #42.888
#비틱가능성템 최소컷
min(helmets[bitik_helm,]$score_nine) #39.5
#평균 9강 점수
mean(helmets$score_nine) #32.87

#평균점수의 차이는 무기와 비교했을떄 유의미할까?
test=t.test(helmets$score_full, weapons$score_full, var.equal=FALSE)
test #significant difference, t(19998)=-8.86, p<.001

#---------------------갑옷-----------------
set.seed(425)
armo=creategear(c(-1, -2, -11)) #-깡방 깡공 공퍼퍼
dim(armo) #8 possibilities+15
armors=samplegears(armo, 8)
armors=gearscore(armors, 8)
#결과는?
mean(armors$score_full) #43.19
max(armors$score_full) #60
which(armors$score_full>55) #장비점수 55점 이상이 99개
which(armors$score_def_full>55) #방어8개
which(armors$score_dta_full>55) #딜탱 10개
which(armors$score_uti_full>55) #유틸 10개
which(armors$score_off_full>45) #50점까진 0, 45점 이상이 11개개
armors[22,] #치피11 속도12 체력16 적중6
#히스토그램
hist(armors$score_full, xlab='장비점수', ylab='갯수', 
     main='영웅등급 갑옷 시뮬 장비점수, N=10000', xlim=c(20,70), col='#FF9966', cex.lab=1.5)
#최종 장비점수 55 이상의 장비는 9강때 몇점일까?
bitik_armo=which(armors$score_full>55)
mean(armors[bitik_armo,]$score_nine) #42.565
#비틱가능성템 최소컷
min(armors[bitik_armo,]$score_nine) #40
mean(armors[which(armors$score_off_full>45),]$score_nine) #공템일 경우에도 40.5
#평균 9강 점수
mean(armors$score_nine) #33.43

#평균점수의 차이는 무기와 비교했을떄 유의미할까?
test=t.test(armors$score_full, weapons$score_full, var.equal=FALSE)
test #NOT significant, t(19998)=.9649, p<.3346

