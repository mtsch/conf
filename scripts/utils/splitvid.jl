function splitvid(args, show=false)
    nameind  = findfirst((x -> x[1] in '0':'9' && x[end] in '0':'9').(args))
    filename = join(args[1:nameind-1], ' ')
    times    = args[nameind:end]
    fn, ext  = splitext(filename)

    for i in 1:length(times)-1

        st = times[i]
        et = times[i+1]

        cmd = `ffmpeg -i $filename -vcodec copy -acodec copy -ss $st -to $et $fn-$i$ext`
        if show
            println(cmd)
        else
            run(cmd)
        end
    end
end

if ARGS[1] == "debug"
    println(ARGS[2:end])
    splitvid(ARGS[2:end], true)
else
    @time splitvid(ARGS[1:end], false)
end
