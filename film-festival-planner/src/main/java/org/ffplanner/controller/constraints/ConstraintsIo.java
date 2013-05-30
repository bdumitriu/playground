package org.ffplanner.controller.constraints;

import org.ffplanner.bean.MovieBundleInFestivalBean;
import org.ffplanner.bean.ShowingBean;
import org.ffplanner.bean.UserScheduleBean;
import org.ffplanner.bean.programme.FestivalEditionProgramme;
import org.ffplanner.def.ConstraintDefinition;
import org.ffplanner.def.ScheduleConstraintsDefinition;
import org.ffplanner.entity.MovieBundleInFestival;
import org.ffplanner.entity.Showing;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * A <span style="color: red">NON PRODUCTION</span> class for writing/reading schedule constraints to/from XML. To be
 * used exclusively for testing.
 *
 * @author Bogdan Dumitriu
 */
public class ConstraintsIo {

    private MovieBundleInFestivalBean movieBundleInFestivalBean;

    private ShowingBean showingBean;

    private UserScheduleBean userScheduleBean;

    public void setMovieBundleInFestivalBean(MovieBundleInFestivalBean movieBundleInFestivalBean) {
        this.movieBundleInFestivalBean = movieBundleInFestivalBean;
    }

    public void setShowingBean(ShowingBean showingBean) {
        this.showingBean = showingBean;
    }

    public void setUserScheduleBean(UserScheduleBean userScheduleBean) {
        this.userScheduleBean = userScheduleBean;
    }

    /**
     * Reads the data from the XML input stream and resets the {@code userSchedule} to the data read.
     * {@link #setUserScheduleBean(UserScheduleBean) setUserScheduleBean} has to be called beforehand.
     */
    public void readToDatabase(
            InputStream inputStream, FestivalEditionProgramme festivalEditionProgramme, Long userId)
            throws IOException, ParserConfigurationException, SAXException {
        final Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(inputStream);
        final NodeList movies = document.getElementsByTagName("movie");
        final NodeList showings = document.getElementsByTagName("showing");
        userScheduleBean.resetConstraints(userId, festivalEditionProgramme.getFestivalEdition());
        new MovieConstraintLoader(userId, festivalEditionProgramme).load(movies);
        new ShowingConstraintLoader(userId).load(showings);
    }

    /**
     * Writes the {@code scheduleConstraints} data to XML format in {@code outputStream}.
     * {@link #setMovieBundleInFestivalBean(MovieBundleInFestivalBean) setMovieBundleInFestivalBean} and
     * {@link #setShowingBean(ShowingBean) setShowingBean} have to be called beforehand.
     */
    public void write(OutputStream outputStream, ScheduleConstraintsDefinition scheduleConstraints) throws IOException {
        try (OutputStreamWriter writer = new OutputStreamWriter(outputStream, StandardCharsets.UTF_8)) {
            writer.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
            writer.append("<schedule-constraints>\n");
            writeMovies(writer, scheduleConstraints);
            writeShowings(writer, scheduleConstraints);
            writer.append("</schedule-constraints>\n");
        }
    }

    private void writeMovies(OutputStreamWriter writer, ScheduleConstraintsDefinition scheduleConstraints)
            throws IOException {
        writer.append("  <movies>\n");
        for (ConstraintDefinition.Movie movieConstraint : scheduleConstraints.getMovieConstraints()) {
            writer.append("    <movie>\n");
            writer.append("      <id>");
            writer.append(String.valueOf(movieConstraint.getMovieId()));
            writer.append("</id>\n");
            writer.append("      <priority>");
            writer.append(String.valueOf(movieConstraint.getPriority()));
            writer.append("</priority>\n");
            if (movieBundleInFestivalBean != null) {
                final MovieBundleInFestival movieBundle = movieBundleInFestivalBean.find(movieConstraint.getMovieId());
                writer.append("      <title>");
                writer.append(movieBundle.getMovieBundle().getOriginalTitle());
                writer.append("</title>\n");
            }
            writer.append("    </movie>\n");
        }
        writer.append("  </movies>\n");
    }

    private void writeShowings(OutputStreamWriter writer, ScheduleConstraintsDefinition scheduleConstraints)
            throws IOException {
        writer.append("  <showings>\n");
        for (ConstraintDefinition.Showing showingConstraint : scheduleConstraints.getShowingConstraints()) {
            writer.append("    <showing>\n");
            writer.append("      <id>");
            writer.append(String.valueOf(showingConstraint.getShowingId()));
            writer.append("</id>\n");
            writer.append("      <priority>");
            writer.append(String.valueOf(showingConstraint.getPriority()));
            writer.append("</priority>\n");
            if (showingBean != null) {
                final Showing showing = showingBean.find(showingConstraint.getShowingId());
                writer.append("      <datetime>");
                writer.append(String.valueOf(showing.getDateTime()));
                writer.append("</datetime>\n");
                writer.append("      <venue-id>");
                writer.append(String.valueOf(showing.getVenueId()));
                writer.append("</venue-id>\n");
                writer.append("      <movie-id>");
                writer.append(String.valueOf(showing.getMovieBundleInFestival().getId()));
                writer.append("</movie-id>\n");
                writer.append("      <title>");
                writer.append(showing.getMovieBundleInFestival().getMovieBundle().getOriginalTitle());
                writer.append("</title>\n");
            }
            writer.append("    </showing>\n");
        }
        writer.append("  </showings>\n");
    }

    private abstract class ConstraintLoader {

        protected Long userId;

        protected ConstraintLoader(Long userId) {
            this.userId = userId;
        }

        protected abstract void setConstraint(Long id, Short priority);

        protected void load(NodeList nodeList) {
            for (int i = 0; i < nodeList.getLength(); i++) {
                final Node node = nodeList.item(i);
                final NodeList childNodes = node.getChildNodes();
                Long id = null;
                Short priority = null;
                for (int j = 0; j < childNodes.getLength(); j++) {
                    final Node childNode = childNodes.item(j);
                    if ("id".equals(childNode.getNodeName())) {
                        id = Long.parseLong(childNode.getFirstChild().getNodeValue());
                    } else if ("priority".equals(childNode.getNodeName())) {
                        priority = Short.parseShort(childNode.getFirstChild().getNodeValue());
                    }
                }
                if (id != null && priority != null) {
                    setConstraint(id, priority);
                }
            }
        }
    }

    private class MovieConstraintLoader extends ConstraintLoader {

        private final FestivalEditionProgramme festivalEditionProgramme;

        protected MovieConstraintLoader(Long userId, FestivalEditionProgramme festivalEditionProgramme) {
            super(userId);
            this.festivalEditionProgramme = festivalEditionProgramme;
        }

        @Override
        protected void setConstraint(Long id, Short priority) {
            final List<Showing> showings = festivalEditionProgramme.getShowingsFor(id);
            if (!showings.isEmpty()) {
                final Showing showing = showings.get(0);
                userScheduleBean.toggleMovieConstraintViaShowing(showing.getId(), userId);
                userScheduleBean.setConstraintPriority(showing.getId(), userId, priority);
            }
        }
    }

    private class ShowingConstraintLoader extends ConstraintLoader {

        protected ShowingConstraintLoader(Long userId) {
            super(userId);
        }

        @Override
        protected void setConstraint(Long id, Short priority) {
            userScheduleBean.toggleShowingConstraint(id, userId);
            userScheduleBean.setConstraintPriority(id, userId, priority);
        }
    }
}
