import vapoursynth as vs
core = vs.get_core()

# skip motion interpolation completely for content exceeding the limits below
max_fps = 60
max_mp = 2.5 * (10**6)  # 2.5 megapixels
# a block is considered as changed when 8*8*x > th_block|flow_diff with 8*8
# being the size of a block (scaled internally) and x the difference of each
# pixel within this block, default 400 (8*8*6.25)
th_block_diff  = int(8*8*6.25)
# (threshold/255)% blocks have to change to consider this a scene change
# (= no motion compensation), default 130
th_block_changed  = 65
# size of blocks the analyse step is performed on
blocksize = 32

# double initial FPS, and let mpv interpolation handle the rest
target_num = container_fps * 2 if container_fps * 2 < max_fps else max_fps
target_num = int(target_num * 1e6)
target_den = int(1e6)
source_num = int(container_fps * 1e6)
source_den = int(1e6)

clip = video_in

if not (clip.width * clip.height > max_mp or container_fps > max_fps):
    clip = core.std.AssumeFPS(clip, fpsnum=source_num, fpsden=source_den)
    sup  = core.mv.Super(clip, pel=2, hpad=blocksize, vpad=blocksize)
    bv   = core.mv.Analyse(sup, blksize=blocksize, isb=True , chroma=True, search=3, searchparam=1)
    fv   = core.mv.Analyse(sup, blksize=blocksize, isb=False, chroma=True, search=3, searchparam=1)
    clip = core.mv.BlockFPS(clip, sup, bv, fv, num=target_num, den=target_den,
                            mode=3, thscd1=th_block_diff, thscd2=th_block_changed)

    print('motion-interpolation: {0} -> {1} FPS'
          .format(source_num / source_den, target_num / target_den))
else:
    print('motion-interpolation: skipping {0}x{1} {2} FPS video'
      .format(clip.width, clip.height, container_fps))

clip.set_output()

# vim: ft=python
