(startdate as any, enddate as any) as table=>
let
    Start_Date = try Date.From(startdate) otherwise #date(2020,01,01),
    Finish_Date = try Date.From(enddate) otherwise #date(2020,02,12),
    #"+Dates" = List.Dates(Date.From(Start_Date), Duration.Days(Duration.From(Finish_Date-Start_Date))+1, #duration(1,0,0,0)),
    #"+DateWeeks" = Table.FromRecords(List.Transform(#"+Dates", (dt)=> 
[Date = dt, 
Weekday = Date.DayOfWeek(dt),
WeekdayName = Date.DayOfWeekName(dt),
Week = Date.WeekOfYear(dt)
]

))
in
    #"+DateWeeks"