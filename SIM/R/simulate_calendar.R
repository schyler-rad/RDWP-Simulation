simulate_calendar=function(){

  loadNamespace('lubridate')

  #define calendar adjustments
  holiday_adjustment=function(calendar,program,holiday_list,adjustment_mat) {

    adjustment=rep(1,length(calendar))

    for (season in names(holiday_list)) {

      adjustment[calendar %in% holiday_list[[season]]]=adjustment_mat[program, season]

    }

    return(adjustment)

  }

  #define calendar creation
  my_cal_fun=function(my_holiday_adjustment_mat,my_dat,my_holiday_list){

    my_holiday_adjustment_mat['playground','summer_break']=sample(seq(.1,1.1,.01),1)
    my_holiday_adjustment_mat['tutoring','summer_break']=sample(seq(.1,1.1,.01),1)
    my_holiday_adjustment_mat['coworking','summer_break']=sample(seq(.1,1.1,.01),1)
    my_holiday_adjustment_mat['cafe','summer_break']=sample(seq(.1,1.1,.01),1)
    #my_holiday_adjustment_mat['party','summer_break']=.9
    #my_holiday_adjustment_mat['workshop','summer_break']=.9
    #my_holiday_adjustment_mat['camp','summer_break']=.9
    #my_holiday_adjustment_mat['merchandise','summer_break']=.9

    my_holiday_adjustment_mat['playground','memorial_day']=sample(seq(.1,1.1,.01),1)
    my_holiday_adjustment_mat['tutoring','memorial_day']=sample(seq(.1,1.1,.01),1)
    my_holiday_adjustment_mat['coworking','memorial_day']=sample(seq(.1,1.1,.01),1)
    my_holiday_adjustment_mat['cafe','memorial_day']=sample(seq(.1,1.1,.01),1)
    #my_holiday_adjustment_mat['party','memorial_day']=.9
    #my_holiday_adjustment_mat['workshop','memorial_day']=.9
    #my_holiday_adjustment_mat['camp','memorial_day']=.9
    #my_holiday_adjustment_mat['merchandise','memorial_day']=.9

    my_holiday_adjustment_mat['playground','labor_day']=sample(seq(.1,1.1,.01),1)
    my_holiday_adjustment_mat['tutoring','labor_day']=sample(seq(.1,1.1,.01),1)
    my_holiday_adjustment_mat['coworking','labor_day']=sample(seq(.1,1.1,.01),1)
    my_holiday_adjustment_mat['cafe','labor_day']=sample(seq(.1,1.1,.01),1)
    #my_holiday_adjustment_mat['party','summer_break']=.9
    #my_holiday_adjustment_mat['workshop','summer_break']=.9
    #my_holiday_adjustment_mat['camp','summer_break']=.9
    #my_holiday_adjustment_mat['merchandise','summer_break']=.9

    my_holiday_adjustment_mat['camp','summer_break']=1.1

    for(program in rownames(my_holiday_adjustment_mat)){

      column_name=paste0("holiday_adjustment_",program)

      my_dat[[column_name]]=holiday_adjustment(calendar=my_dat$date,
                                               program=program,
                                               holiday_list=my_holiday_list,
                                               adjustment_mat=my_holiday_adjustment_mat
      )

    }

    my_dat$rainfall_adjustment=rainfall_adjustment[my_dat$month]

    my_dat$weekend_adjustment_playground=ifelse(my_dat$weekday %in% c('Saturday','Sunday'),sample(seq(.9,1.9,.01),1),1)
    my_dat$weekend_adjustment_tutoring=ifelse(my_dat$weekday %in% c('Saturday','Sunday'),sample(seq(.1,1.1,.01),1),1)
    my_dat$weekend_adjustment_coworking=ifelse(my_dat$weekday %in% c('Saturday','Sunday'),sample(seq(.1,1.1,.01),1),1)
    my_dat$weekend_adjustment_cafe=ifelse(my_dat$weekday %in% c('Saturday','Sunday'),sample(seq(.9,1.9,.01),1),1)
    my_dat$weekend_adjustment_party=ifelse(my_dat$weekday %in% c('Saturday','Sunday'),sample(seq(.9,1.9,.01),1),1)
    my_dat$weekend_adjustment_workshop=ifelse(my_dat$weekday %in% c('Saturday','Sunday'),sample(seq(.1,1.1,.01),1),1)
    my_dat$weekend_adjustment_camp=ifelse(my_dat$weekday %in% c('Saturday','Sunday'),sample(seq(.1,1.1,.01),1),1.1)
    my_dat$weekend_adjustment_merchandise=ifelse(my_dat$weekday %in% c('Saturday','Sunday'),sample(seq(.9,1.9,.01),1),.9)
    #my_dat$weekend_adjustment_merchandise=ifelse(my_dat$day %in% c('Saturday','Sunday'),1.1,1)

    return(my_dat)

  }

  #simulate calendar
  begin=as.Date("2025/01/01")
  end=as.Date("2025/12/31")

  holiday_list=list(winter_break=seq.Date(as.Date("2025-12-20"),as.Date("2025-12-31"),by="day"),
                    spring_break=seq.Date(as.Date("2025-03-15"),as.Date("2025-03-22"),by="day"),
                    summer_break=seq.Date(as.Date("2025-06-15"),as.Date("2025-08-31"),by="day"),
                    fall_break=seq.Date(as.Date("2025-11-24"),as.Date("2025-11-28"),by="day"),
                    mlk_day=as.Date("2025-01-20"),
                    presidents_day=as.Date("2025-02-17"),
                    memorial_day=as.Date("2025-05-26"),
                    labor_day=as.Date("2025-09-01")#,

                    #teacher_workday=c(as.Date("2025-01-06"), as.Date("2025-10-13")),
                    #extra_holiday=seq.Date(as.Date("2025-05-01"), as.Date("2025-05-05"), by="day")

  )

  rainfall_adjustment=c(1.1541,1.1006,1.1122,
                        1.0725,1.0446,1.0288,
                        1.0105,1.019,1.0403,
                        1.1002,1.1622,1.1551)

  my_seq=seq.Date(begin,end,"days")

  dat=data.frame(date=my_seq,month=lubridate::month(my_seq),day=lubridate::day(my_seq),weekday=weekdays(my_seq))

  holiday_adjustment_mat=matrix(1.1,nrow=8,ncol=length(holiday_list),
                                dimnames=list(c('playground','tutoring','coworking','cafe',
                                                'party','workshop','camp','merchandise'),c(names(holiday_list))))

  my_calendar=my_cal_fun(holiday_adjustment_mat,dat,holiday_list)

  return(my_calendar)

}
