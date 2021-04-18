(tbl as table) as table =>
let
    debug_tbl = Table.FromRecords({[uuid = "bc468fe4-07ec-44be-8ec8-d3f8c37bb7bc",job_run_id = "tb_cohort",load_dttm = "2021-04-11T12:48:38.7904654",upd_ind = "I",cohort_id = "2021_Q1",year = Number.FromText("2021"),start_date = Date.FromText("1/1/2021"),end_date = Date.FromText("31/3/2021")],[uuid = "28c6d7f8-6196-49a0-a21a-4cac3efdbed4",job_run_id = "tb_cohort",load_dttm = "2021-04-11T12:48:38.7904654",upd_ind = "I",cohort_id = "2021_Q2",year = Number.FromText("2021"),start_date = Date.FromText("1/4/2021"),end_date = Date.FromText("30/6/2021")],[uuid = "e485a31f-d3eb-418f-8bfc-7602bf44d282",job_run_id = "tb_cohort",load_dttm = "2021-04-11T12:48:38.7904654",upd_ind = "I",cohort_id = "2021_Q3",year = Number.FromText("2021"),start_date = Date.FromText("1/7/2021"),end_date = Date.FromText("30/9/2021")],[uuid = "5668a526-4495-42b5-b70f-20003f9de9f4",job_run_id = "tb_cohort",load_dttm = "2021-04-11T12:48:38.7904654",upd_ind = "I",cohort_id = "2021_Q4",year = Number.FromText("2021"),start_date = Date.FromText("1/10/2021"),end_date = Date.FromText("31/12/2021")]}),

    Source = try tbl otherwise debug_tbl,
    #"Header Row" = Table.FromRecords({Record.FromList(Table.ColumnNames(Source), Table.ColumnNames(Source))}),
    #"Row List" = Table.TransformRows(Table.AddIndexColumn(Source, "Index", 1), (rec)=> "Row " & Text.From(rec[Index])),
    #"Transposed Table" = Table.Transpose(Table.Combine({#"Header Row", Source}), List.Combine({{"Header"}, #"Row List"}))
in
    #"Transposed Table"