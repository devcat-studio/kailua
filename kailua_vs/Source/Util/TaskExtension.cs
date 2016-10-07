using System;
using System.Threading.Tasks;

namespace Kailua.Util.Extensions
{
    public static class TaskExtension
    {
        public static Task<Result> CreateSyncTask<Result>(this Func<Result> job)
        {
            var completionSource = new TaskCompletionSource<Result>();
            try
            {
                completionSource.SetResult(job());
            }
            catch (Exception e)
            {
                completionSource.SetException(e);
            }
            return completionSource.Task;
        }

        public static Task<Result> CreateSyncTask<Arg1, Result>(this Func<Arg1, Result> job, Arg1 arg1)
        {
            return CreateSyncTask<Result>(() => job(arg1));
        }

        public static Task<Result> CreateSyncTask<Arg1, Arg2, Result>(this Func<Arg1, Arg2, Result> job, Arg1 arg1, Arg2 arg2)
        {
            return CreateSyncTask<Result>(() => job(arg1, arg2));
        }

        public static Task<Result> CreateSyncTask<Arg1, Arg2, Arg3, Result>(this Func<Arg1, Arg2, Arg3, Result> job, Arg1 arg1, Arg2 arg2, Arg3 arg3)
        {
            return CreateSyncTask<Result>(() => job(arg1, arg2, arg3));
        }

        public static Task<Result> CreateSyncTask<Arg1, Arg2, Arg3, Arg4, Result>(this Func<Arg1, Arg2, Arg3, Arg4, Result> job, Arg1 arg1, Arg2 arg2, Arg3 arg3, Arg4 arg4)
        {
            return CreateSyncTask<Result>(() => job(arg1, arg2, arg3, arg4));
        }
    }
}
