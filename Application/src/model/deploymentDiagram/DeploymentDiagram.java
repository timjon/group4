package model.deploymentDiagram;

import model.sequenceDiagramParser.Meta;
import java.util.List;

/**
 * Stores deployment diagram data to be used by the GSON library.
 * @author Sebastian Fransson
 * @version 1.0
 */
public class DeploymentDiagram {
    public Meta meta;
    public String type;
    public List<Mapping> mapping;
}
