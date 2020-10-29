using ArgParse

function resizevid(file, reso, thresh, debug, clean)
    println("========================================")
    print(file)
    fn, ext = splitext(file)

    sz = wt = ht = ""
    try
        sz = read(`ffprobe -v error -of flat=s=_ -select_streams v:0 -show_entries stream=height,width $file`, String)
        wt = parse(Int, split(split(sz, '\n')[1], '=')[2])
        ht = parse(Int, split(split(sz, '\n')[2], '=')[2])
    catch e
        println(e)
        return false
    end
    outfile = "$fn-$reso$ext"
    largesize = min(wt, ht)

    println(" [$(wt)×$(ht)]")

    if (reso + thresh <= largesize)
        resstr = wt > ht ? "-2:$reso" : "$reso:-2"
        loglvl = "error"
        cmd    = `ffmpeg -hide_banner -v $loglvl -i $file -vf scale=$resstr -preset fast -c:v libx265 -x265-params log-level=$loglvl -async 1 -vsync 1 $outfile`
        if debug
            println(cmd)
        else
            try
                print("resizing...")
                run(cmd)
                println(" OK")
                if clean
                    rm(file)
                end
                return true
            catch e
                println(" FAIL: $e")
                rm("$fn-$reso$ext")
                return false
            end
        end
    else
        false
    end
end

function parseargs()
    s = ArgParseSettings()
    @add_arg_table s begin
        "-r"
        help = "recursive"
        action = :store_true
        "--res"
        help = "target resolution"
        arg_type = Int
        default = 540
        "--thr"
        help = "reso threshold"
        arg_type = Int
        default = 70
        "-c"
        help = "clean"
        action = :store_true
        "-d"
        help = "debug"
        action = :store_true
        "vid"
        help = "target video"
        arg_type = String
        nargs = '+'
    end
    parse_args(ARGS, s, as_symbols = true)
end

function main()
    args = parseargs()
    file = join(args[:vid], ' ')
    resize = f -> resizevid(f, args[:res], args[:thr], args[:d], args[:c])

    if isdir(file) && args[:r]
        for (root, dir, files) in walkdir(file)
            for f in files
                resize(joinpath(root, f))
            end
        end
    elseif isdir(file)
        for f in filter(isfile, readdir(file))
            resize(joinpath(file, f))
        end
    else
        resize(joinpath(file))
    end

    println("\nFINITO.")
end

if !isinteractive()
    main()
end
