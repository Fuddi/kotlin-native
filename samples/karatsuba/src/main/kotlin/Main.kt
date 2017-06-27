import kotlin.system.measureTimeMicros
import kotlin.system.measureTimeMillis

val  aBox : Array<Long> = arrayOf<Long>(0x7d8d, 0x54ca, 0x1866, 0x3399, 0x26d1, 0x2d0d, 0xcaf9, 0xf169, 0xbbce, 0xd1a8, 0xd51f, 0x4ecd,
        0xd035, 0xd7f8, 0x1cbb, 0xc278, 0xe6dc, 0xbeb3, 0xaa99, 0x3b75, 0xee36, 0x3629, 0x7787, 0xd4e0, 0x5882, 0x6965, 0x733b, 0x4ed5, 0x6c08,
        0x70f3, 0x5614, 0x83dc, 0x016b, 0, 0, 0);
val bBox : Array<Long> = arrayOf<Long>(0x577d, 0x11fd, 0x7740, 0xfa10, 0xc40e, 0x54cb, 0xbb90, 0xb69f, 0xb805, 0x0214, 0x211e, 0x666e, 0x5ccc, 0x5be4, 0xd1be, 0x0344,
        0x3e08, 0xb277, 0x3c8a, 0x1d5a, 0x6df7, 0x95a5, 0xb110, 0xe0ae, 0x1e01, 0x2420, 0x0b9d, 0x8f3a, 0x7cae, 0x9d93, 0x616f, 0x8e71, 0x01b7, 0, 0, 0)

val  aNonBox : LongArray = longArrayOf(0x7d8d, 0x54ca, 0x1866, 0x3399, 0x26d1, 0x2d0d, 0xcaf9, 0xf169, 0xbbce, 0xd1a8, 0xd51f, 0x4ecd,
        0xd035, 0xd7f8, 0x1cbb, 0xc278, 0xe6dc, 0xbeb3, 0xaa99, 0x3b75, 0xee36, 0x3629, 0x7787, 0xd4e0, 0x5882, 0x6965, 0x733b, 0x4ed5, 0x6c08,
        0x70f3, 0x5614, 0x83dc, 0x016b, 0, 0, 0);
val bNonBox : LongArray = longArrayOf(0x577d, 0x11fd, 0x7740, 0xfa10, 0xc40e, 0x54cb, 0xbb90, 0xb69f, 0xb805, 0x0214, 0x211e, 0x666e, 0x5ccc, 0x5be4, 0xd1be, 0x0344,
        0x3e08, 0xb277, 0x3c8a, 0x1d5a, 0x6df7, 0x95a5, 0xb110, 0xe0ae, 0x1e01, 0x2420, 0x0b9d, 0x8f3a, 0x7cae, 0x9d93, 0x616f, 0x8e71, 0x01b7, 0, 0, 0)

/** Parameters (all are optional): <Box|NonBox> <iterations per run>, <warm up runs>, <runs>. */
fun main(args : Array<String>) {

    val karatsuba : Karatsuba = when(args.getOrNull(0)?.toUpperCase()) {
        "BOX" -> KaratsubaBox(1, aBox, bBox)
        "NONBOX" -> KaratsubaNonBox(1, aNonBox, bNonBox)
        else -> KaratsubaNonBox(1, aNonBox, bNonBox)
    }

    val iterations = args.getOrNull(1)?.toInt() ?: 100000
    val warmUpRuns = args.getOrNull(2)?.toInt() ?: 2
    val runs       = args.getOrNull(3)?.toInt() ?: 10

    println("Start benchmarking: ${karatsuba.name} with $warmUpRuns warm up runs, $runs runs (each $iterations iterations)")

    // Warm up
    for (run in 0 until warmUpRuns) {
        var avgTime: Long = 0
        for (i in 0 until iterations) {
            avgTime += measureTimeMicros {
                val result: Any = karatsuba.compute()
            }
        }
        println("Warm up run #$run. Time: ${(avgTime.toDouble() / iterations)} us")
    }
    println()

    // Run
    for (run in 0 until runs) {
        var avgTime: Long = 0
        for (i in 0 until iterations) {
            avgTime += measureTimeMicros {
                val result: Any = karatsuba.compute()
            }
        }
        println("Run #$run. Time: ${(avgTime.toDouble() / iterations)} us")
    }
}