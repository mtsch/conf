using ArgParse

function resizevid(targeth, filename, thresh, debug)
    fn, ext = splitext(filename)

    sz = readstring(`ffprobe -v error -of flat=s=_ -select_streams v:0 -show_entries stream=height,width $filename`)
    ht = parse(Int, split(split(sz, '\n')[2], '=')[2])

    if (targeth + thresh <= ht)
        cmd = `ffmpeg -i $filename -vf scale=-1:$targeth -analyzeduration 6M -qscale 5 $fn-$targeth$ext`
        println(cmd)
        if !debug
            try
                @time _ = readstring(cmd)
                println("Success.")
                true
            catch
                rm("$fn-$targeth$ext")
                println("Failed.")
                false
            end
        else
            false
        end
    else
        println("Nothing to be done, video is $(ht)p.")
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
        default = ["."]
        nargs = '*'
    end
    parse_args(ARGS, s, as_symbols = true)
end

if !isinteractive()
    args  = parseargs()
    file  = join(args[:vid], ' ')
    file  = replace(file, "\\ ", " ")
    reso  = args[:res]
    recur = args[:r]
    clean = args[:c]
    debug = args[:d]
    thr   = args[:thr]

    if recur && !isdir(file)
        println(STDERR, "Not a directory!")
    elseif !recur && !isfile(file)
        println(STDERR, "Not a file!")
    elseif recur
        try
            for (root, dirs, files) in walkdir(file)
                for (i, f) in enumerate(files)
                    println("Resizing $i/$(length(files))")
                    succ = resizevid(reso, f, thr, debug)
                    succ && clean && rm(f)
                end
            end
        end
    else
        try
            succ = resizevid(reso, file, thr, debug)
            succ && clean && rm(file)
        end
    end

    println("========")
    println("FINISHED")
    println("========")
end
