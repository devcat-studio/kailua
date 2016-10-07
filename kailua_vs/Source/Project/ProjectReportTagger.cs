using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using Kailua.Util.Extensions;

namespace Kailua
{
    [Export(typeof(ITaggerProvider))]
    [ContentType("kailua")]
    [TagType(typeof(ReportTag))]
    internal sealed class ProjectReportTagProvider : ITaggerProvider
    {
        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            // one TokenTagger instance is unique to given ITextBuffer instance
            // (otherwise TagAggregator is unable to receive an event from it)
            return buffer.Properties.GetOrCreateSingletonProperty(delegate() { return new ProjectReportTagger(buffer); }) as ITagger<T>;
        }
    }

    internal sealed class ProjectReportTagger : ITagger<ReportTag>
    {
        ITextBuffer buffer;
        SortedDictionary<Project, List<Native.ReportData>> reportsPerProject;

        internal ProjectReportTagger(ITextBuffer buffer)
        {
            this.buffer = buffer;
            this.reportsPerProject = new SortedDictionary<Project, List<Native.ReportData>>();

            // XXX for now, only subscribe to the projects which initially contained the current file
            string fileName = buffer.GetFilePath();
            if (fileName != null)
            {
                foreach (var project in ProjectCache.GetProjects(fileName))
                {
                    project.ReportDataChanged += delegate(IList<Native.ReportData> reportData)
                    {
                        reportsPerProject[project] = reportData.ToList();

                        // TagsChanged callback should run in the UI thread
                        ThreadHelper.JoinableTaskFactory.RunAsync(async delegate()
                        {
                            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                            if (this.TagsChanged != null)
                            {
                                var snapshot = this.buffer.CurrentSnapshot;
                                var span = new SnapshotSpan(snapshot, 0, snapshot.Length);
                                this.TagsChanged(this, new SnapshotSpanEventArgs(span));
                            }
                        });
                    };
                }
            }
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        IEnumerable<ITagSpan<ReportTag>> ITagger<ReportTag>.GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if (spans.Count <= 0)
            {
                yield break;
            }

            var snapshot = this.buffer.CurrentSnapshot;
            Debug.Assert(snapshot == spans[0].Snapshot);

            String fileName = this.buffer.GetFilePath() ?? "";
            foreach (var entry in this.reportsPerProject)
            {
                var project = entry.Key;
                foreach (var data in entry.Value)
                {
                    var reportSpan = data.SnapshotSpanNonEmpty;
                    if (!reportSpan.HasValue)
                    {
                        continue;
                    }

                    // reportSpan may refer to the incorrect snapshot
                    var span = reportSpan.Value.TranslateTo(snapshot, SpanTrackingMode.EdgeExclusive);
                    if (spans.IntersectsWith(span))
                    {
                        // checker errors go through a separate error list, but the error tagger itself
                        // will duplicate them into its own error list unless disabled.
                        var tag = new ReportTag(data, fileName, displayInErrorList: false);
                        yield return new TagSpan<ReportTag>(span, tag);
                    }
                }
            }
        }
    }
}
